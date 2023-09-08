---
title: "SSH over HTTPS"
date: August 30, 2023
---

Tl;DR: to pass `SSH` through `HTTPS` you need to tweak client and server
sides as the following:

Example client entry in `~/.ssh/config`:

```
# $ cat .ssh/config
Host ssh-via-https
        ProxyCommand ~/.ssh/https-tunnel.bash
        # some firewalls aggressively close idle TCP connections
        ServerAliveInterval 30
```

The `~/.ssh/https-tunnel.bash` helper script we use above:

```
#!/usr/bin/env bash
{ printf "CONNECT ssh-server:22 HTTP/1.0\r\n\r\n"; cat; } | socat - SSL:https-server:443
```

Example server entry for `apache2` `HTTPS`:

```
# $ cat /etc/httpd/httpd.conf
LoadModule proxy_connect_module .../modules/mod_proxy_connect.so
# ...
AllowCONNECT 22
<Proxy *>
    Order deny,allow
    Deny from all
</Proxy>
<Proxy ssh-server>
    Order deny,allow
    Allow from all
</Proxy>
```

Here we allow everyone to use `CONNECT` `HTTP` method on the server side
hosted at `https-server` just for a single target: the `ssh-server` host.

And on the client side we use `socat` to create `TLS` connection with a
sent `CONNECT` method as a header.

Now you can use `$ ssh ssh-via-https` to reach `ssh-server`.

More words below:

## Background

Why do I need it?

I planned to spend 1-2 days in the hospital and did not plan to use the
laptop.But now I am stuck here for the past two weeks and would like to
tinker on small stuff remotely. The hospital has free Wi-fi access.

The caveat is that hospital blocks most connection types. It allows you
only to do `HTTP` (`TCP` port `80`) and `HTTPS` (`TCP` port `443`)
connections for most hosts. `DNS` (`UDP` port `53`) and `DoT` (`TCP`
port `853`) seem to work as well at least for well-known `DNS`
providers.

But `SSH` (`TCP` port `22` or most other custom ports) is blocked
completely.

I wondered how hard would it be to pass `SSH` through `HTTP` or `HTTPS`.
I had a `GSM` fallback so I could reconfigure remote server and try
various solutions.

## The options

There are various avenues to explore here:

1. Co-host `SSH` and `HTTPS` protocols on a single port and dispatch
   them transparently. [sslh project](https://github.com/yrutschle/sslh)
   does just that.

   One of it's features (or limitations) is that it does not encapsulate
   one protocol in another: it guesses the protocol using various
   heuristics and redirects it to the actual backend: `SSH` server,
   `HTTPS` or any other supported protocol from
   [the list](https://github.com/yrutschle/sslh/blob/master/probe.c#L50).

   Clear pro here is:

   - no special `ssh` client configuration is required: you just specify
     non-default port.

   The minor cons are:

   - and extra service to set up
   - `HTTPS` has to be passed through `sslh` as well, which might hide
     source address of the connections for the backend. Not convenient
     for logging (and possibly performance?).

2. Encapsulate one protocol completely into another.

  `openssh` supports `ProxyCommand` directive effectively allowing user to
  supply any transport that `ssh` protocol uses:

  ```
  Specifies the command to use to connect to the server. The command string extends to the end of the line, and is executed using the user's shell ‘exec’ directive to avoid a lingering shell process.
  Arguments to ProxyCommand accept the tokens described in the TOKENS section. The command can be basically anything, and should read from its standard input and write to its standard output. It should eventually connect an sshd(8) server running on some machine, or execute sshd -i somewhere. Host key management will be done using the Hostname of the host being connected (defaulting to the name typed by the user). Setting the command to none disables this option entirely. Note that CheckHostIP is not available for connects with a proxy command.

  This directive is useful in conjunction with nc(1) and its proxy support. For example, the following directive would connect via an HTTP proxy at 192.0.2.0:

  ProxyCommand /usr/bin/nc -X connect -x 192.0.2.0:8080 %h %p
  ```

  Clear pros here:

  - full encapsulation of one protocol in another: no heuristics needed,
    harder to block by a Deep Packet Inspection software.
  - `HTTPS` itself is not impacted or redirected

  The cons might be also substantial:

  - possible performance decrease due to double-encryption
  - requires client configuration to use proxy

## SSH over HTTP

I went with the option `[2.]`: filly encapsulate `SSH` protocol into
`TLS`.

Before doing that I tried a simpler `SSH` over `HTTP`.

I configured `apache2` to allow `CONNECT` method for a single target
`ssh-server:22` as:

```
$ cat /etc/httpd/httpd.conf
LoadModule proxy_connect_module .../modules/mod_proxy_connect.so
# ...
AllowCONNECT 22
<Proxy *>
    Order deny,allow
    Deny from all
</Proxy>
<Proxy ssh-server>
    Order deny,allow
    Allow from all
</Proxy>
```

The client side was trivial to adapt using `socat`:

```
$ cat .ssh/config
Host ssh-via-http
        Hostname ssh-server
        Port 22
        ProxyCommand socat - PROXY:http-server:%h:%p,proxyport=80
        ServerAliveInterval 30
```

Using the above `$ ssh ssh-via-http` command is enough to use port `80`
to connect to `SSH`.

I use `ServerAliveInterval` to workaround some kind of silent idle
`HTTP` connection closure somewhere in transit: I tell `openssh`
client to send periodic probes to the server to make `HTTP` connection
look alive. That way an idle session can stay alive for longer periods
of time.

By default `httpd` uses default `TimeOut 60` seconds of no I/O as a
signal to close the tunnel.

## SSH over HTTPS

For some reason `socat` does not support `HTTPS` proxy via `CONNECT`
method and only supports `HTTP`. A minor omission? Or I hold `socat`
wrong?

Luckily `socat` does support `TLS` encapsulation. `CONNECT`-based
method is easy to implement manually. I did it via one-line script:

```
$ cat ~/.ssh/https-tunnel.bash
#!/usr/bin/env bash
{ printf "CONNECT ssh-server:22 HTTP/1.0\r\n\r\n"; cat; } | socat - SSL:https-server:443
```

Now we can use this script as is for `SSH` over `HTTPS` tunneling:

```
$ cat .ssh/config
Host ssh-via-https
        ProxyCommand ~/.ssh/https-tunnel.bash
        # some firewalls aggressively close idle TCP connections
        ServerAliveInterval 30
```

And `$ ssh ssh-via-https` does the expected thing.

The result allowed me to get to my home machine and write this blog post.

## Parting words

Ubiquitous presence of `HTTPS` allows you to pass your data through very
restrictive middle boxes!

The `CONNECT` method while seemingly being a remnant of the far past is
a useful hack to wrap any `TCP` payload stream into `TLS` host stream.

The `ServerAliveInterval` `openssh` knob allows you to keep the
connection alive if underlying transport is not friendly to idle
connections.

Have fun!
