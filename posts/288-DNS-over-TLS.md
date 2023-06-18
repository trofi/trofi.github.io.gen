---
title: "DNS over TLS"
date: June 18, 2023
---

A few months ago I finally got optical internet connection with native
`IPv6` . This prompted me to slightly reconfigure my devices: I
abandoned [HE.net's tunnel](https://tunnelbroker.net/) as my `IPv6`
outlet.

I explored my DNS hardening as well. At first I flipped
a `DNSSEC` and `DNSOverTLS` on for `systemd-resolved` without much
thought and got it to do something. The result seemed to work.

Later I noticed that `netstat` takes a while to resolve local `IPv6`
addresses back to domain names: it took 10+ seconds per printed line.

This seems to be a reproducer:

```
$ dig slyfox.uni.cx AAAA | grep -v '^;'
slyfox.uni.cx.          37      IN      AAAA    2a00:23c8:a613:101:7912:b70d:1c91:ab02

$ time dig -x 2a00:23c8:a613:101:7912:b70d:1c91:ab02
;; communications error to 127.0.0.53#53: timed out
;; communications error to 127.0.0.53#53: timed out
;; communications error to 127.0.0.53#53: timed out

; <<>> DiG 9.18.14 <<>> -x 2a00:23c8:a613:101:7912:b70d:1c91:ab02
;; global options: +cmd
;; no servers could be reached

real    0m15.028s
user    0m0.002s
sys     0m0.005s
```

Reverse resolve was not only slow: it also did not return a reasonable
`DNS` response (`NXDOMAIN` or similar). This means result did not get
cached at all and subsequent run was as slow.

Why did slowness start happening? `IPv4`-only world with `HE.net` tunnel
had no such problems.

I have effectively external `IPv6` address for some internet facing
resources. They are in the same subnet as my desktop! This makes local
reverse resolution to be very slow.

`systemd-resolved` as is does not provide detailed enough logs. I had to
`strace` it to see what `DNS` servers and protocols it talks to. Not
very convenient.

`strace` revealed that `systemd-resolved` sends some of reverse lookup
requests to link-specific `DNS` server which happens to be my ISP router
(over `DoT` port which router does not support). Other requests are sent
to globally configured `DNS`. That was a surprise.

Disabling `DNSOverTLS=yes` option also did not help: the router still
did not want to resolve or forward the requests upstream.

On top of that `systemd-resolved` did not keep a `TCP` connection to
`DoT` servers. As a result each (even successful) resolve
takes a while due to a 3-way `TCP` (and `TLS`?) handshake. This adds
about 10ms to each uncached query.

I wanted a bit more flexibility where my `DNS` requests go. Thus I
switched over to a familiar `unbound` package. `unbound` does keep `TCP`
connections to `DoT` servers (which makes even uncached `DNS` requests
under 10ms). Debugging story is also more straightforward: specifying
`verbosity: ...` parameter in the config is enough to see what is being
resolved and how forwards happen.

On `NixOS` the naive switch to `unbound` is a few lines of service
setup:

```nix
{ ... }:
{
  # Slow at resolving reverse loopups for IPv6, like:
  #   $ dig -x 2002:...
  services.resolved.enable = false;

  services.unbound = {
    enable = true;

    settings = {
      forward-zone = [
        {
          name = ".";
          forward-tls-upstream = "yes";
          forward-addr = [
            "2001:4860:4860::8888@853#dns.google"
            "2001:4860:4860::8844@853#dns.google"
            "8.8.8.8@853#dns.google"
            "8.8.4.4@853#dns.google"
          ];
        }
      ];
    };
  };
}
```

Most of "code" here is to specify `DoT`-capable recursive servers. The
above expands to the following `unbound` configuration:

```
$ cat /etc/unbound/unbound.conf
server:

  access-control: 127.0.0.0/8 allow
  access-control: ::1/128 allow
  auto-trust-anchor-file: /var/lib/unbound/root.key
  chroot: ""
  directory: /var/lib/unbound
  do-daemonize: no
  interface: 127.0.0.1
  interface: ::1
  ip-freebind: yes
  pidfile: ""
  tls-cert-bundle: /etc/ssl/certs/ca-certificates.crt
  username: unbound
forward-zone:
  forward-addr: 2001:4860:4860::8888@853#dns.google
  forward-addr: 2001:4860:4860::8844@853#dns.google
  forward-addr: 8.8.8.8@853#dns.google
  forward-addr: 8.8.4.4@853#dns.google
  forward-tls-upstream: yes
  name: .
remote-control:
  control-cert-file: /var/lib/unbound/unbound_control.pem
  control-enable: no
  control-interface: 127.0.0.1
  control-interface: ::1
  control-key-file: /var/lib/unbound/unbound_control.key
  server-cert-file: /var/lib/unbound/unbound_server.pem
  server-key-file: /var/lib/unbound/unbound_server.key
```

Now all my direct and reverse queries are nice and quick:

```
$ time dig -x 2a00:23c8:a613:101:7912:b70d:1c91:ab02
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NXDOMAIN, id: 6507

;; AUTHORITY SECTION:
0.a.2.ip6.arpa.         1274    IN      SOA     pri.authdns.ripe.net. dns.ripe.net. 1687019637 3600 600 864000 3600

;; Query time: 0 msec
;; SERVER: 127.0.0.1#53(127.0.0.1) (UDP)
;; WHEN: Sat Jun 17 22:15:27 BST 2023
;; MSG SIZE  rcvd: 161
```

WARNING: do not use these settings as is until you fully understand the
implications of abandoning your default resolver. Non-exhaustive list of
things to note:

- some of local requests are forwarded to the public `DNS`: you might
  need to tweak your local zones a bit more if you want to prevent
  leaking our resolution requests related to machines in you network

- all your non-local requests to to a single entity: you might now want
  to send all your DNS queries to a single public `DNS` server for
  privacy reasons

- `mDNS` is disabled and your local resources might stop resolving as is

I have local workarounds for all of these. Your setup will likely be
different.

When picking among `DoT` public servers I was choosing between:

- `Cloudflare`: `1.1.1.1` and similar
- `Google`: `8.8.8.8` and similar
- `Quad9` `9.9.9.9` and similar

These are all for-profit companies. Your priorities might prompt you to
evaluate other `DoT`-capable servers.

Anyway. Of the above somehow `Cloudflare` latency is consistently 2x-3x
slower for initial `DoT` setup than the other two:

```
$ dig +tls @1.1.1.1 kernel.org
;; ANSWER SECTION:
kernel.org.             100     IN      A       139.178.84.217
;; Query time: 50 msec
;; SERVER: 1.1.1.1#853(1.1.1.1) (TLS)

$ dig +tls @8.8.8.8 kernel.org
;; ANSWER SECTION:
kernel.org.             175     IN      A       139.178.84.217
;; Query time: 14 msec
;; SERVER: 8.8.8.8#853(8.8.8.8) (TLS)

$ dig +tls @9.9.9.9 kernel.org
;; ANSWER SECTION:
kernel.org.             289     IN      A       139.178.84.217
;; Query time: 19 msec
;; SERVER: 9.9.9.9#853(9.9.9.9) (TLS)
```

Maybe `Cloudflare` does not have a close enough `TLS` termination near
me? `mtr -4 -T -P 853` says that both `1.1.1.1` and `8.8.8.8` are 4.5ms
away from me, while `9.9.9.9` is 9ms away from me. Non-TLS queries are
as performant `mtr`-reported values:

```
$ dig @1.1.1.1 kernel.org
;; ANSWER SECTION:
kernel.org.             52      IN      A       139.178.84.217

;; Query time: 5 msec
;; SERVER: 1.1.1.1#53(1.1.1.1) (UDP)

$ dig +tcp @8.8.8.8 kernel.org
;; ANSWER SECTION:
kernel.org.             92      IN      A       139.178.84.217

;; Query time: 5 msec
;; SERVER: 8.8.8.8#53(8.8.8.8) (TCP)

$ dig +tcp @9.9.9.9 kernel.org
;; ANSWER SECTION:
kernel.org.             36      IN      A       139.178.84.217

;; Query time: 9 msec
;; SERVER: 9.9.9.9#53(9.9.9.9) (TCP)
```

I settled on `dns.google` (`8.8.8.8` and backups).

## Parting words

`systemd-resolved` needs a bit of handholding to make reverse resolves
to work quickly. `unbound` is a bit easier to get up and running.

`DoT`'s latency tax is around 2-3x as it needs more RTTs supposedly for
`TLS`-related machinery. If your resolver is close enough it's not too
bad.

In theory `DNS-over-QUIC` could shrink latency further down to levels of
unencrypted `DNS`. One `DoQ` gains more popularity and it's support
gets added to `unbound`: <https://github.com/NLnetLabs/unbound/issues/743>.

Have fun!
