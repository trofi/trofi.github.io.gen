---
title: "CONNECT passthrough on nginx"
date: May 25, 2026
---

[A while ago](/posts/295-ssh-over-https.html) I explored easy hacks on
how to encapsulate `SSH` into `HTTPS` on `apache2`. Since then I moved a few
my systems from `apache2` to `nginx` to play a bit with `http/2` and
`http/3`. I wanted to port my hack to `nginx`.

At the time `nginx` did not have direct support for `CONNECT` handling.
I used [`chobits/ngx_http_proxy_connect_module`](https://github.com/chobits/ngx_http_proxy_connect_module)
external module to implement `CONNECT`. But around the end of 2025 the
extension stopped compiling against latest `nginx` versions and I
dropped `CONNECT` hack.
This extension is not meant to be portable across `nginx` versions: it
applies invasive patches to `nxing` core to add method support and tends
to break with minor `nginx` updates.

This week I wondered how can I get `CONNECT` handling back. I explored a
few other `HTTP` servers and found out that `nginx-2.31.0` does have
native support for `CONNECT` starting with `2.31.0` as a
[`ngx_http_tunnel_module`](https://nginx.org/en/docs/http/ngx_http_tunnel_module.html)!
The [admin guide](https://docs.nginx.com/nginx/admin-guide/web-server/http-connect-proxy/)
provided a simple example of setting it up.

## The `nginx` Config Change

Here is a configuration example to match `nginx` variant of passing
through `ssh` connection to a single target. Suppose you have a simple
`nginx` config to handle static files as:

```nginx
http {
        server {
                listen 80 default_server;
                listen [::]:80 default_server;
                listen 443 ssl;
                listen [::]:443 ssl;
                http2 on;

                // ... server_name and ssl_ stuff go here

                location / {
                        root /path/to/public_html;
                        index index.html;
                }
        }
}
```

To get `CONNECT` for a limited set of targets it's a matter of adding
`tunnel_pass` under an allowed condition:

```diff
--- before.conf
+++ after.conf
@@ -1,16 +1,25 @@
 http {
+        map "$request_method:$host:$request_port" $allow_connect_host_port {
+                "CONNECT:ssh-server:22" 1;
+        }
         server {
                 listen 80 default_server;
                 listen [::]:80 default_server;
                 listen 443 ssl;
                 listen [::]:443 ssl;
                 http2 on;

                 // ... server_name and ssl_ stuff go here

                 location / {
                         root /path/to/public_html;
                         index index.html;
+
+                        # CONNECT handing
+                        resolver 127.0.0.1 [::1];
+                        if ($allow_connect_host_port = 1) {
+                                tunnel_pass;
+                        }
                 }
         }
 }
```

As `nginx` is slightly stricter about the used protocol than `apache2`
it requires client to connect as `HTTP/1.1` and as a result pass the
`Host:` header. Thus, the client side looks this way.
`~/.ssh/https-tunnel.bash`:

```bash
#!/usr/bin/env bash

{ printf "CONNECT $1:$2 HTTP/1.1\r\nHost: $1\r\n\r\n"; cat; } | socat - "SSL:$3:$4"
```

`.ssh/config`:

```
Host ssh-via-http
        Hostname ssh-server
        Port 22
        ProxyCommand socat - PROXY:http-server:%h:%p,proxyport=80
        ServerAliveInterval 30
```

## Parting Words

`nginx-1.31.0` got has native support for `CONNECT` pass through for
both `HTTP` and `HTTPS` via
[`ngx_http_tunnel_module`](https://nginx.org/en/docs/http/ngx_http_tunnel_module.html).
