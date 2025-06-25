---
layout: default
title: bowline
rank: 2
---

# `bowline`

`bowline` is a DNS full resolver (cache server) which supports:

* Basic feature of DNS full resolvers
* DNS over UDP 53 and TCP 53
* DNS over TLS, QUIC, H2 and H3 for communication with stub resolvers
* DNSSEC for communication with authoritative servers
* Nagative Trust Anchor
* stub-zone, local-zone, local-data
* DNSTAP for logging
* Web API (for reloading etc)

## Configuration

* [`bowline.conf`](https://github.com/iijlab/dnsext/blob/main/dnsext-bowline/bowline/bowline.conf)
* [`local-example.conf`](https://github.com/iijlab/dnsext/blob/main/dnsext-bowline/bowline/local-example.conf)
* [`stub-example.conf`](https://github.com/iijlab/dnsext/blob/main/dnsext-bowline/bowline/stub-example.conf)

## Executing

```
% sudo bowline bowline.conf
```

If `bowline` runs well, a monitor console is provided via stdin/stdout. Type `help` to know its commands.

## Web API

Send `GET` to the following path:

* `/metrics`: get stats
* `/stats`: get stats
* `/wstats`: TBD
* `/reopen-log`: reopen a log file
* `/reload`: reload
* `/keep-cache`: Reloading with the cache kept
* `/quit`: quit
* `/help`: display help
* `/`: display help
