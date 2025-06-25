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

## Usage

```
% sudo bowline bowline.conf
```

## Configuration

* [`bowline.conf`](https://github.com/iijlab/dnsext/blob/main/dnsext-bowline/bowline/bowline.conf).
* [`local-example.conf`](https://github.com/iijlab/dnsext/blob/main/dnsext-bowline/bowline/local-example.conf).
* [`stub-example.conf`](https://github.com/iijlab/dnsext/blob/main/dnsext-bowline/bowline/stub-example.conf).
