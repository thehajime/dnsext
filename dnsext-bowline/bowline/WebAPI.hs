{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module WebAPI (
    bindAPI,
    run,
) where

import qualified Control.Exception as E
import Data.ByteString ()
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.String
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Header (hContentType, hContentDisposition)
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp hiding (run)
import System.Posix (getSystemID, nodeName)

import DNS.Iterative.Server (withLocationIOE)

import Config
import Types

import Text.Heredoc (heredocFile)

doStats :: Control -> IO Response
doStats Control{..} = responseBuilder HTTP.ok200 [] <$> getStats

doWStats :: Control -> IO Response
doWStats Control{..} = responseBuilder HTTP.ok200 [] <$> getWStats

doMacosProf :: IO Response
doMacosProf = do
  hostname <- nodeName <$> getSystemID
  let txt = fromString $ xmls hostname

  return $ responseBuilder HTTP.ok200 [
    (hContentType, "application/xml; charset=UTF-8"),
      (hContentDisposition, "attachment; filename=bowline.mobileconfig")
    ] txt
  where
    uuidDoH = fromString $ "0F54C4EF-5D3D-47B7-AC34-21E2F307D69E"
    uuidDoT = fromString $ "03B5987F-3EFD-4479-97CF-591D469B3F00"
    uuidMain = fromString $ "0E6EDCAE-81BF-4A8B-85D7-7BC3D031EC76"
    xmls hostname = $(heredocFile "./bowline/template.mobileconfig")

{- FOURMOLU_DISABLE -}
doHelp :: IO Response
doHelp = return $ responseBuilder HTTP.ok200 [] txt
  where
    txt = fromString $ unlines $ "WebAPI help:" : "" : map (uncurry hline) helps
    helps =
        [ ("/metrics"     , "returns metrics info")
        , ("/wstats"      , "returns worker thread info")
        , ("/reopen-log"  , "reopen logfile when file logging")
        , ("/reload"      , "reload bowline without keeping cache")
        , ("/keep-cache"  , "reload bowline with keeping cache")
        , ("/macos-prof"  , "download macos DNSsettings profile for this resolver")
        , ("/quit"        , "quit bowline")
        , ("/help"        , "show this help texts")
        ]
    hline name note = name ++ replicate (width - length name) ' ' ++ note
    width = maximum (0 : map (length . fst) helps) + margin
    margin = 3
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
app :: Control -> Application
app mng req sendResp = getResp >>= sendResp
  where
    getResp
        | requestMethod req == HTTP.methodGet = case rawPathInfo req of
            "/metrics"     -> doStats mng
            "/stats"       -> doStats mng
            "/wstats"      -> doWStats mng
            "/reopen-log"  -> reopenLog mng $> ok
            "/reload"      -> reloadCmd mng Reload    failed ok
            "/keep-cache"  -> reloadCmd mng KeepCache failed ok
            "/macos-prof"  -> doMacosProf
            "/quit"        -> quitCmd mng Quit $> ok
            "/help"        -> doHelp
            "/"            -> doHelp
            _ -> return $ ng HTTP.badRequest400
        | otherwise = return $ ng HTTP.methodNotAllowed405
{- FOURMOLU_ENABLE -}

ok :: Response
ok = responseLBS HTTP.ok200 [] "OK\n"

failed :: Response
failed = responseLBS HTTP.ok200 [] "FAILED\n"

ng :: HTTP.Status -> Response
ng st = responseLBS st [] "NG\n"

{- FOURMOLU_DISABLE -}
bindAPI :: Config -> IO (Maybe Socket)
bindAPI Config{..}
    | cnf_webapi  = resolve >>= open <&> Just
    | otherwise   = return Nothing
  where
    resolve = do
        let hints =
                defaultHints
                    { addrFlags = [AI_PASSIVE, AI_NUMERICHOST, AI_NUMERICSERV]
                    , addrSocketType = Stream
                    }
        NE.head <$> getAddrInfo (Just hints) (Just cnf_webapi_addr) (Just $ show cnf_webapi_port)
    open ai@AddrInfo{addrAddress = a} = E.bracketOnError (openSocket ai) close $ \sock -> do
        withLocationIOE (show a ++ "/webapi") $ do
            setSocketOption sock ReuseAddr 1
            withFdSocket sock setCloseOnExecIfNeeded
            bind sock a
            listen sock 32
        return sock
{- FOURMOLU_ENABLE -}

run :: Control -> Socket -> IO ()
run mng sock = E.finally (runSettingsSocket defaultSettings sock $ app mng) (close sock)
