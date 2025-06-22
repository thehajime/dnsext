{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (
    atomically,
    newTBQueueIO,
    readTBQueue,
    writeTBQueue,
 )
import qualified Control.Exception as E
import Control.Monad (forever, void, when)
import Data.ByteString (ByteString)
import Data.ByteString.Short ()
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IP ()
import Data.List (intercalate, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (fromString)
import GHC.Conc.Sync (
    ThreadStatus,
    labelThread,
    listThreads,
    threadLabel,
    threadStatus,
 )
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Console.GetOpt (
    ArgDescr (..),
    ArgOrder (..),
    OptDescr (..),
    getOpt,
    usageInfo,
 )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Log.FastLogger
import System.Posix.Signals

import DNS.Do53.Client (
    LookupConf (..),
    LookupEnv,
    Seeds (..),
    defaultCacheConf,
    defaultLookupConf,
    withLookupConf,
 )
import DNS.Do53.Internal (
    NameTag (..),
    Reply (..),
    ResolveActions (..),
    ResolveInfo (..),
    Resolver,
 )
import DNS.DoX.Client (
    SVCBInfo (..),
    lookupSVCBInfo,
    modifyForDDR,
    toPipelineResolver,
 )
import DNS.SEC (addResourceDataForDNSSEC)
import DNS.SVCB (ALPN, addResourceDataForSVCB)
import DNS.Types (
    DNSMessage (..),
    Domain,
    Question (..),
    ResourceRecord (..),
    runInitIO,
    toRepresentation,
 )
import DNS.Types.Decode (decode)
import DNS.Types.Encode (encode)

----------------------------------------------------------------

data Options = Options
    { optHelp :: Bool
    , optDebug :: Bool
    , optALPN :: Maybe ALPN
    }

defaultOptions :: Options
defaultOptions =
    Options
        { optHelp = False
        , optDebug = False
        , optALPN = Nothing
        }

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['h']
        ["help"]
        (NoArg (\opts -> opts{optHelp = True}))
        "print help"
    , Option
        ['d']
        ["debug"]
        (NoArg (\opts -> opts{optDebug = True}))
        "print debug info"
    , Option
        ['a']
        ["alpn"]
        (ReqArg (\s o -> o{optALPN = Just (fromString s)}) "<alpn>")
        "specify ALPN to select SVCB entries"
    ]

----------------------------------------------------------------

usage :: String
usage = "Usage: ddrd [OPTION] ipaddr [ipaddr...]"

showUsageAndExit :: IO a
showUsageAndExit = do
    putStrLn $ usageInfo usage options
    exitFailure

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, _errs) -> showUsageAndExit

----------------------------------------------------------------

serverAddr :: String
serverAddr = "127.0.0.1"

serverPort :: String
serverPort = "53"

----------------------------------------------------------------

serverResolve :: HostName -> ServiceName -> IO AddrInfo
serverResolve addr port = NE.head <$> getAddrInfo (Just hints) (Just addr) (Just port)
  where
    hints =
        defaultHints
            { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV, AI_PASSIVE]
            , addrSocketType = Datagram
            }

serverSocket :: AddrInfo -> IO Socket
serverSocket ai = E.bracketOnError (openSocket ai) close $ \s -> do
    setSocketOption s ReuseAddr 1
    bind s $ addrAddress ai
    return s

----------------------------------------------------------------

printDebug :: Options -> String -> IO ()
printDebug opts msg = when (optDebug opts) $ putStrLn msg

pprDomain :: Domain -> String
pprDomain = init . toRepresentation

pprRR :: ResourceRecord -> String
pprRR ResourceRecord{..} = pprDomain rrname ++ " " ++ show rrtype ++ " " ++ show rdata

----------------------------------------------------------------

numberOfWorkers :: Int
numberOfWorkers = 10

labelMe :: String -> IO ()
labelMe lbl = myThreadId >>= \tid -> labelThread tid lbl

----------------------------------------------------------------

data Op a = Op
    { enqueue :: a -> IO ()
    , dequeue :: IO a
    , send :: a -> IO ()
    , recv :: IO a
    , wait :: IO ()
    , putLog :: LogStr -> IO ()
    }

main :: IO ()
main = do
    labelMe "ddrd main"
    args <- getArgs
    (opts, ips) <- parseOpts args
    when (optHelp opts) showUsageAndExit
    when (null ips) showUsageAndExit
    runInitIO $ do
        addResourceDataForDNSSEC
        addResourceDataForSVCB
    let logtyp
            | optDebug opts = LogStderr 1024
            | otherwise = LogNone
    (putL, killLogger) <- newFastLogger1 logtyp
    setHandlers putL
    ai <- serverResolve serverAddr serverPort
    E.bracket (serverSocket ai) close $ \s -> do
        q <- newTBQueueIO 128
        let op =
                Op
                    { enqueue = \bssa -> atomically $ writeTBQueue q bssa
                    , dequeue = atomically $ readTBQueue q
                    , send = \(bs, sa) -> void $ NSB.sendTo s bs sa
                    , recv = NSB.recvFrom s 2048
                    , wait = do
                        wait' <- waitReadSocketSTM s
                        atomically wait'
                    , putLog = putL
                    }
        void $ forkIO $ do
            labelMe "reader"
            reader op
        ref <- newIORef Map.empty
        let conf = makeConf ref ips
        withLookupConf conf $ mainLoop opts op
    killLogger

----------------------------------------------------------------

reader :: Op (ByteString, SockAddr) -> IO ()
reader Op{..} = forever (recv >>= enqueue)

worker :: Op (ByteString, SockAddr) -> Resolver -> IO ()
worker Op{..} resolver = do
    mytid <- myThreadId
    labelThread mytid "worker"
    let tid = drop 9 $ show mytid
    forever $ do
        (bs, sa) <- dequeue
        case decode bs of
            Left _ -> putLog "Decode error\n"
            Right msg -> case question msg of
                [] -> putLog "No questions\n"
                qry : _ -> do
                    putLog $ toLogStr $ tid ++ " Q: " ++ pprDomain (qname qry) ++ " " ++ show (qtype qry) ++ "\n"
                    let idnt = identifier msg
                    erep <- resolver qry mempty
                    case erep of
                        Left _ -> putLog "No reply\n"
                        Right rep -> do
                            let msg' =
                                    (replyDNSMessage rep)
                                        { identifier = idnt
                                        }
                            putLog $ toLogStr $ tid ++ " R: " ++ intercalate "\n   " (map pprRR (answer msg')) ++ "\n"
                            void $ send (encode msg', sa)

mainLoop :: Options -> Op (ByteString, SockAddr) -> LookupEnv -> IO ()
mainLoop opts op@Op{..} env = loop
  where
    unsafeHead [] = error "unsafeHead"
    unsafeHead (x : _) = x
    loop = do
        wait
        er <- lookupSVCBInfo env
        case er of
            Left e -> do
                putLog $ toLogStr $ show e ++ "\n"
                threadDelay 3000000
            Right siss -> case selectSVCB (optALPN opts) siss of
                Nothing -> do
                    putLog "SVCB RR is not available\n"
                    threadDelay 3000000
                Just si -> do
                    let ri = unsafeHead $ svcbInfoResolveInfos si
                    putLog $
                        toLogStr $
                            "Running a pipeline resolver on " ++ show (svcbInfoALPN si) ++ " " ++ show (rinfoIP ri) ++ " " ++ show (rinfoPort ri) ++ "\n"
                    let piplineResolver = unsafeHead $ toPipelineResolver si
                    E.handle ignore $ piplineResolver $ \resolver -> do
                        let runWorkers = foldr1 concurrently_ $ replicate numberOfWorkers $ worker op resolver
                        runWorkers
        loop
    ignore (E.SomeException se) = putLog $ toLogStr $ show se

----------------------------------------------------------------

selectSVCB :: Maybe ALPN -> [[SVCBInfo]] -> Maybe SVCBInfo
selectSVCB (Just alpn) siss = modifyForDDR <$> listToMaybe (filter (\s -> svcbInfoALPN s == alpn) (concat siss))
selectSVCB Nothing ((si : _) : _) = Just $ modifyForDDR si
selectSVCB _ _ = Nothing

----------------------------------------------------------------

makeConf
    :: IORef (Map.Map NameTag ByteString)
    -> [String]
    -> LookupConf
makeConf ref addrs =
    defaultLookupConf
        { lconfCacheConf = Just defaultCacheConf
        , lconfConcurrent = True
        , lconfSeeds = SeedsAddrs $ map read addrs
        , lconfActions =
            actions
                { ractionUseEarlyData = True
                , ractionValidate = True
                , ractionResumptionInfo = \tag -> do
                    m <- readIORef ref
                    case Map.lookup tag m of
                        Nothing -> return []
                        Just x -> return [x]
                , ractionOnResumptionInfo = \tag bs ->
                    atomicModifyIORef' ref $ \m -> (Map.insert tag bs m, ())
                }
        }
  where
    actions = lconfActions defaultLookupConf

----------------------------------------------------------------

threadSummary :: IO [(String, String, ThreadStatus)]
threadSummary = (sort <$> listThreads) >>= mapM summary
  where
    summary t = do
        let idstr = drop 9 $ show t
        l <- fromMaybe "(no name)" <$> threadLabel t
        s <- threadStatus t
        return (idstr, l, s)

setHandlers :: (LogStr -> IO ()) -> IO ()
setHandlers putLog = do
    void $ installHandler sigUSR1 infoHandler Nothing
  where
    infoHandler = Catch $ do
        labelMe "USR1 signale handler"
        threadSummary >>= mapM_ (putLog . toLogStr . showT)
      where
        showT (i, l, s) = i ++ " " ++ l ++ ": " ++ show s ++ "\n"
