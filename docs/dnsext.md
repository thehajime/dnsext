---
layout: default
title: dnsext-*
rank: 5
---

# `dnsext-*`

This is a new series of DNS libraries based on the experience of the [dns](https://github.com/kazu-yamamoto/dns) library in Haskell. The dns library has two flaws:

- Resource records are not extensible
- Resource records are not friendly to caching

Resource records are implemented as a sum type. The third party library cannot extend them. The only way to extend them is to send a pull request to the dns library.

Some resource records use `ByteString` internally. So, if they are cached for a long time, fragmentation happens.

This new library uses typeclasses to extend resource records and uses `ShortByteString` in them.

* `dnsext-types`: basic types
* `dnsext-dnssec`: DNSSEC
* `dnsext-svcb`: SVCB
* `dnsext-utils`: utility functions
* `dnsext-do53`: DNS over UDP 53 and TCP 53
* `dnsext-dox`: DNS over TLS, QUIC, H2 and H3
* `dnsext-iterative`: iterative queries
* `dnsext-bowline`: `bowline`, `dug` and `ddrd`
