---
title: guix on gentoo howto
date: February 11, 2017
---

## TL;DR

Getting `guix` on `gentoo`:

- install `guix` from `gentoo` main tree: `emerge guix`
- enable `guix-daemon` service:
  - on `systemd`: `systemctl enable guix-daemon`
  - on `openrc`: `rc-update add guix-daemon`
- create a profile symlink and set some environment variables:

  ```
  # profiles and PATH
  $ ln -sf /var/guix/profiles/per-user/$USER/guix-profile $HOME/.guix-profile
  $ export PATH="${HOME}/.guix-profile/bin:${HOME}/.guix-profile/sbin${PATH:+:}$PATH"

  # gnutls certs
  $ export SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs"
  $ export SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
  $ export GIT_SSL_CAINFO="$SSL_CERT_FILE"

  # libc locales
  $ export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
  ```

Ready!

## More commands

- install a package: `guix package -i icecat`
- uninstall a package: `guix package -r icecat`
- start a new shell with only specified package(s) in environment:
  `guix environment --ad-hoc --pure icecat`

## Some words on `guix`

`guix` has the same underlying storage model as `nix`: package descriptions
fully capture environment of inputs to get reproducible build result of
built artifact. They are also called derivations.

Command line interface is slightly different from `nix`: to me it feels
more consistent and more powerful. For example `nix` provides tools like
`nix-build`, `nix-env`, `nix-collect-garbage`, `nix-store`,
`nix-shell`. While `guix` provides single entry point `guix` command.
`guix environment` (sibling of `nix-shell`) allows you to create an
isolated shell and optionally run it in a separate container. That
provides additional isolation layer on foreign distros (I run `gentoo`).
Example comparison to get `ghc` in isolated environment:

```
# get only 'ghc' into environemnt
$ nix-shell --pure -p ghc


# same but for guix
$ guix environment --ad-hoc ghc
[sf] ~:echo /bin/a*
/bin/attr /bin/awk

# same as above but in container
$ guix environment --ad-hoc --container ghc
slyfox@sf ~ [env]# echo /bin/a*
/bin/a*
```

Here `container` mode allows us to hide all specifics of a host
system. It gives me more confidence that nothing from my host system
leaks into `guix`-built packages.
Let's look at an example `guix` package. One of tools missing in `guix`
repos was `re2c`. I've dumped the following into current directory:

```scheme
; cat ~/.guix/re2c.scm
(use-modules (guix)
             (guix build-system gnu)
             (guix licenses))

(package
  (name "re2c")
  (version "0.15")
  (source (origin
           (method url-fetch)
           (uri (string-append "https://github.com/skvadrik/" name
                               "/releases/download/" version "/"
                               name "-" version ".tar.gz"))
           (sha256
            (base32
             ; got as: guix download https://github.com/skvadrik/re2c/releases/download/0.15/re2c-0.15.tar.gz
             "1kdfjs6jb1d4rymk8aqr3l775jx6hl8ml9wpv3q23i2vyfxnnsas"))))
  (build-system gnu-build-system)
  (home-page "http://re2c.org/")
  (synopsis "Lexer generator for C/C++")
  (description
   "@code{re2c} generates minimalistic ...")
  (license public-domain))
```

Trying to build the package:

```
$ guix build -f re2c.scm re2c
substitute: updating list of substitutes from 'https://mirror.hydra.gnu.org'... 100.0%
@ build-started /gnu/store/1nwww0i3cgp59yyc0r9wsg98ynmj6dq8-re2c-0.15.drv - x86_64-linux /var/log/guix/drvs/1n//www0i3cgp59yyc0r9wsg98ynmj6dq8-re2c-0.15.drv.bz2
starting phase `set-SOURCE-DATE-EPOCH'
...
phase `compress-documentation' succeeded after 0.0 seconds
@ build-succeeded /gnu/store/1nwww0i3cgp59yyc0r9wsg98ynmj6dq8-re2c-0.15.drv -
/gnu/store/8mcnjdismqmg25ds9jg7rf6ay3vwlqxr-re2c-0.16
/gnu/store/wfmj24p5y3xva5jrc8rrk25z9zf6ssvx-re2c-0.15
```

Done!

We can install built package with `guix package -f re2c.scm -i re2c`
or run it in isolated environment: `guix environment -l re2c.scm
--ad-hoc -C re2c`
The package was added upstream in [this
form](http://git.savannah.gnu.org/cgit/guix.git/commit/gnu/packages/re2c.scm?id=cc1c3977d54728280ec6649e1883912b1226e63f)
Once package got accepted into `guix` tree everyone is able to get it with
mere `guix pull`. We can verify that local build matches hydra build:

```
$ guix build --check re2c
...
/gnu/store/8mcnjdismqmg25ds9jg7rf6ay3vwlqxr-re2c-0.16
```

No errors so far. Let's check size of runtime dependencies:

```
$ guix size re2c
store item                                                       total    self
/gnu/store/8mcnjdismqmg25ds9jg7rf6ay3vwlqxr-re2c-0.16               61.4     0.4   0.6%
/gnu/store/cdi08kw7r6r684w8mk0xq0dkgpjhfpmd-gcc-4.9.4-lib           61.0    22.7  37.0%
/gnu/store/iwgi9001dmmihrjg4rqhd6pa6788prjw-glibc-2.24              38.3    36.8  60.0%
/gnu/store/rvgmixpmsq5lqr9qflhkm70kg7a4rys2-bash-static-4.4.0        1.4     1.4   2.3%
total: 61.4 MiB
```

Another useful feature is to get build logs on hydra. Say, how did build
look like on `arm`? Easy:

```
$ guix build --log-file re2c --system=armhf-linux
substitute: updating list of substitutes from 'https://mirror.hydra.gnu.org'... 100.0%
https://mirror.hydra.gnu.org/log/j6hlxb9bbvi8wvjcxj8mswkcdkjy8kc8-re2c-0.16
...
configure flags: ("CONFIG_SHELL=/gnu/store/dbsflrccll1laf2q0asr6gl995b9p7y7-bash-4.4.0/bin/bash" "SHELL=/gnu/store/dbsflrccll1laf2q0asr6gl995b9p7y7-bash-4.4.0/bin/bash" "--prefix=/gnu/store/j6hlxb9bbvi8wvjcxj8mswkcdkjy8kc8-re2c-0.16" "--enable-fast-install" "--build=arm-unknown-linux-gnueabihf")
...
checking size of char... 1
checking size of short... 2
checking size of int... 4
checking size of long... 4
checking size of long long... 8
checking size of __int64... 0
checking size of void *... 4
checking size of 0l... 4
checking size of 0ll... 8
checking size of 0i8... 0
```

Looks like `arm`. There is many more interesting topics in `guix` user
manual: <https://www.gnu.org/software/guix/manual/>

Some of them are:

- visualise dependency graphs with `guix graph`
- check for problems in packages (including known vulnerabilities) with
  `guix lint`
- importing packages from other systems like `cpan`, `hackage`, `nix` and
  others
- checking for upstream updates of known packages with `guix refresh`
- bootstrapping of a new architecture or OS

Have fun!
