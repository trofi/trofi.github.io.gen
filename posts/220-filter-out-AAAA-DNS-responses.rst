---
title: "Filter out AAAA DNS responses"
date: November 07, 2020
---

:PostID: 220
:Title: "Filter out AAAA DNS responses"
:Keywords: unbound, ipv6, AAAA
:Categories: notes

Tl;DR: with RPZ we can hide AAAA records while preserving other records
-----------------------------------------------------------------------

For those who look up the way to filter out AAAA records
in their local caching DNS server here is my **unbound**
snippet for a single **ipv6** address:

.. code-block::

    $ cat /etc/unbound/rpz.home
    $ORIGIN rpz.home.
    ;; fails to resolve: https://bugs.gentoo.org/742326
    ;; block 2607:fcc0:4:ffff::4/128 -> NODATA (CNAME *.)
    128.4.0.0.0.ffff.4.fcc0.2607.rpz-ip.rpz.home. CNAME *.

    $ cat /etc/unbound/unbound.conf
    server:
        ... usual configuration goes here
        module-config: "respip validator iterator"

    rpz:
        name: rpz.home.
        zonefile: /etc/unbound/rpz.home

Resolver can't see the **ipv6** address, only **ipv4**:

.. code-block::

    $ dig bugs.gentoo.org AAAA @8.8.8.8
    gannet.gentoo.org.      21599   IN      AAAA    2607:fcc0:4:ffff::4
    $ dig bugs.gentoo.org AAAA | fgrep AAAA
    # nothing

    $ dig bugs.gentoo.org A @8.8.8.8
    gannet.gentoo.org.      21599   IN      A       204.187.15.4
    $ dig bugs.gentoo.org A
    gannet.gentoo.org.      20091   IN      A       204.187.15.4

You can do full subnetworks as well.

Why?
----

The other day I tried to reach **bugs.gentoo.org** programmatically
over HTTP API and observed traffic blackholing. **wget** was also
showing hangups:

.. code-block::

    $ wget bugs.gentoo.org
    --2020-09-19 09:24:22--  http://bugs.gentoo.org/
    Resolving bugs.gentoo.org... 2607:fcc0:4:ffff::4, 204.187.15.4
    Connecting to bugs.gentoo.org|2607:fcc0:4:ffff::4|:80...
    # hung up
    ^C

    $ wget -4 bugs.gentoo.org
    --2020-09-19 09:25:02--  http://bugs.gentoo.org/
    Resolving bugs.gentoo.org... 204.187.15.4
    Connecting to bugs.gentoo.org|204.187.15.4|:80... connected.
    HTTP request sent, awaiting response... 301 Moved Permanently
    ...
    2020-09-19 09:25:03 (1.08 MB/s) - 'index.html' saved [13934]

As a workaround I added **AAAA** response filtering locally as
specified in **TL;DR** to unblock the scripts.
