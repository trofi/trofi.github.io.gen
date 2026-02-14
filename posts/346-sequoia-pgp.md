---
title: "sequoia pgp"
date: February 14, 2026
---

## TL;DR

If you are a `PGP` newbie (like me) then consider reading
[`sq user documentation`](https://book.sequoia-pgp.org/) book! It gave
me an idea of how to use the `sq` tool and how `PGP` concepts map to it.
The book also has a "Background" section that I missed so much to get a
better grasp of `PGP` model.

## Story mode

I created my first `PGP` key in 2008:

```
$ gpg --list-public-keys slyich@gmail.com
pub   dsa1024/0x71A1EE76611FF3AA 2008-10-18 [SC] [revoked: 2018-07-04]
      9929AD151B96AF651958D35871A1EE76611FF3AA
uid                   [ revoked] Sergei Trofimovich <slyfox@...>
uid                   [ revoked] Sergei Trofimovich <st@...>
uid                   [ revoked] Sergei Trofimovich <slyfox@...>
uid                   [ revoked] Sergei Trofimovich <slyich@...>
uid                   [ revoked] Sergei Trofimovich <slyfox@...>
uid                   [ revoked] Sergei Trofimovich <siarheit@...>

pub   rsa4096/0x44FE231F3F3926E4 2018-07-03 [SC] [expires: 2027-08-18]
      62197C11C7C25A61C448E95644FE231F3F3926E4
uid                   [ultimate] Sergei Trofimovich <slyich@...>
sub   rsa4096/0xBA6C2FC245B4DF2C 2018-07-03 [E] [expires: 2027-08-18]
sub   rsa4096/0xED5E45E06F2AC293 2018-07-03 [S] [expires: 2027-08-18]
```

You could tell I had no idea how to use it then even by looking at the
key! No subkeys, long list of attached identities some of which were
stale, I revoked `DSA-1024` key years after algorithm was officially
declared weak.

Looking back into the mail history I suspect I started using `PGP`
after Mikhail's suggestion. Mikhail always introduced me to the new
fancy things he recently found. Be it `SoftICE`, `Gmail` beta, `XMPP`,
`Wave`, `GitHub` and infinite list of other things I already forgot.

Fast forward into 2018 `Gentoo` updated [`GLEP 63`](https://www.gentoo.org/glep/glep-0063.html)
and started requiring all the devs to have an `RSA` key, use keys with
expiration dates and discourage `DSA` key usage. This made my `2008` key
invalid. I had to generate a new key and picked `RSA-4096`. I either did
not follow an equivalent of
[modern guide](https://wiki.gentoo.org/wiki/Project:Infrastructure/Generating_GLEP_63_based_OpenPGP_keys)
or it did not exist at the time. As a result I got very slow commit
signing experience :)

Even 10 years after I started using `PGP` I had almost no mental model
of what a key vs a subkey is. How both relate to private key (or keys?).
How does Web of Trust work. How to make
sure you don't export to much to the keyservers. Why editing the
expiration date does not change the key itself.
All I read at the time was
[The GNU Privacy Handbook](https://www.gnupg.org/gph/en/manual.html)
from 1999. From what I understand it was not updated since.

I used `gnupg` for key management and occasional file decryption. Email
clients had a decent `PGP` UI integration to be easily usable. But many
non-tech users were confused to see signature attaches and tried to
download and unpack it. I stopped using email signing by default.

I felt that `gnupg` as a tool was not very user friendly: it has a ton
of options and interactive questions that I have no idea how to answer
confidently.

Recent [`Fedora` and `GPG` 2.5](https://lwn.net/Articles/1055053/) `LWN`
article from 2026 tricked me into looking at `Sequoia PGP`. Having heard
a bit of `LibrePGP` vs `OpenPGP` story it made me wonder: would `sq` tool
allow me to get a bit better mental model of basic `PGP` concepts and
ability to introspect keys and messages?

## Trying out `sq`

I read [`sq user documentation`](https://book.sequoia-pgp.org/) and I
strongly recommend reading it to get both the idea of `PGP` basics and
`sq` specifics on how to do trivial things!

Here is what `sq` has to say about my (private) `PGP` keys:

```
$ sq key list
 - Backend softkeys has no keys.

 - 62197C11C7C25A61C448E95644FE231F3F3926E4
   - user IDs:
     - Sergei Trofimovich <slyich@...> (authenticated)
     - Sergei Trofimovich <slyfox@...> (UNAUTHENTICATED) revoked
   - created 2018-07-03 08:06:04 UTC
   - will expire 2027-08-18T20:45:35Z
   - usable for signing and decryption
   - @gpg-agent/default: available, locked

   - B6E7C10B37726D7DF059BFE7BA6C2FC245B4DF2C
     - created 2018-07-03 08:06:04 UTC
     - will expire 2027-08-18T20:45:44Z
     - usable for signing and decryption
     - @gpg-agent/default: available, locked
   - FA0D7526A27870BE3842498DED5E45E06F2AC293
     - created 2018-07-03 19:19:15 UTC
     - will expire 2027-08-18T20:46:23Z
     - usable for signing and decryption
     - @gpg-agent/default: available, locked

 - 9929AD151B96AF651958D35871A1EE76611FF3AA
   - user IDs:
     - Sergei Trofimovich <siarheit@...> (UNAUTHENTICATED)
     - Sergei Trofimovich <slyfox@...> (UNAUTHENTICATED)
     - Sergei Trofimovich <slyfox@...> (UNAUTHENTICATED)
     - Sergei Trofimovich <slyfox@...> (UNAUTHENTICATED)
     - Sergei Trofimovich <slyich@...> (UNAUTHENTICATED)
     - Sergei Trofimovich <st@...> (UNAUTHENTICATED)
   - created 2008-10-18 10:28:05 UTC
   - revoked on 2018-07-04 19:35:27 UTC, Key is superseded: Migrated to new more secure key 62197C11C7C25A61C448E95644FE231F3F3926E4
   - not valid: Policy rejected asymmetric algorithm: DSA1024 is not considered secure since 2014-02-01T00:00:00Z
   - usable for signing
   - @gpg-agent/default: available, locked

   - BD21D77765C9B8A655EAC11B8F20BA89A99E563C
     - created 2008-10-18 10:28:05 UTC
     - usable for decryption
     - @gpg-agent/default: available, locked
```

This command shown me outright which identities I revoked in my current
key and they were wrong! (I fixed it since). If nothing else that was a
nice side-effect of trying `sq`.

I find this verbose output slightly more readable at least as a
first-time user. It shows a bit more detail on advertised algorithms in
the keys, violated security policies for outdated algorithms.

## Other random bits

`sq network search` allows for a key lookup and gets keys into the cache
without any explicit assignment of trustworthiness to them.

`sq pki link add` and `sq pki authenticate` allow for a lighter-weight
way of tracking the key authenticity locally without the need to export
your relation to other IDs.

I'll not spend too much time here, but the book mentions nice things
like implementation bits of `WKD` and `DANE` to support `PGP`
infrastructure.

## Introspection: `sq introspect` and `sq dump`

`sq inspect` is a nice tool to explore the `PGP` keys and `PGP` messages.

Encrypting the message:

```
$ sq encrypt --signer-email=slyich@gmail.com --for-email slyich@gmail.com foo --output foo.pgp
Composing a message...

 - encrypted for Sergei Trofimovich <slyich@gmail.com> (authenticated)
   - using 62197C11C7C25A61C448E95644FE231F3F3926E4

 - signed by Sergei Trofimovich <slyich@gmail.com> (authenticated)
   - using 62197C11C7C25A61C448E95644FE231F3F3926E4
```

Exploring the content:

```
$ sq inspect foo.pgp
foo.pgp: Encrypted OpenPGP Message.

      Recipient: BA6C2FC245B4DF2C
        Associated certificate:
          62197C11C7C25A61C448E95644FE231F3F3926E4
          Sergei Trofimovich <slyich@gmail.com> (authenticated)
```

I think the equivalent `gpg` command is `gpg --list-packets`:

```
$ gpg --list-packets foo.pgp
gpg: encrypted with rsa4096 key, ID 0xBA6C2FC245B4DF2C, created 2018-07-03
      "Sergei Trofimovich <slyich@gmail.com>"

<asks for password>

gpg: using "0xED5E45E06F2AC293" as default secret key for signing
# off=0 ctb=c1 tag=1 hlen=3 plen=523 new-ctb
:pubkey enc packet: version 3, algo 1, keyid BA6C2FC245B4DF2C
        data: [4088 bits]
# off=526 ctb=d2 tag=18 hlen=3 plen=729 new-ctb
:encrypted data packet:
        length: 729
        mdc_method: 2
# off=548 ctb=c4 tag=4 hlen=2 plen=13 new-ctb
:onepass_sig packet: keyid 44FE231F3F3926E4
        version 3, sigclass 0x00, digest 10, pubkey 1, last=1
# off=563 ctb=cb tag=11 hlen=2 plen=10 new-ctb
:literal data packet:
        mode b (62), created 0, name="",
        raw data: 4 bytes
# off=575 ctb=c2 tag=2 hlen=3 plen=658 new-ctb
:signature packet: algo 1, keyid 44FE231F3F3926E4
        version 4, created 1771059670, md5len 0, sigclass 0x00
        digest algo 10, begin of digest f2 08
        critical hashed subpkt 2 len 4 (sig created 2026-02-14)
        hashed subpkt 16 len 8 (issuer key ID 44FE231F3F3926E4)
        hashed subpkt 20 len 70 (notation: salt@notations.sequoia-pgp.org=[not human readable])
        hashed subpkt 33 len 21 (issuer fpr v4 62197C11C7C25A61C448E95644FE231F3F3926E4)
        hashed subpkt 35 len 21 (?)
        data: [4096 bits]
```

It's a lot more verbose than `sq inspect` (that's nice!). But is it
obvious what algorithm was used to encrypt the message? One probably
needs to know algorithm numbers like `algo 1` or `digest algo 10`.

An `sq` equivalent would be `sq packet` invocation:

```
$ sq packet dump foo.pgp
Public-Key Encrypted Session Key Packet, new CTB, 523 bytes
    Version: 3
    Recipient: BA6C2FC245B4DF2C
    Pk algo: RSA

Sym. Encrypted and Integrity Protected Data Packet, new CTB, 729 bytes
│   Version: 1
│   Session key: 477E1CA59418C21B6D95AB78B129EED8A53888447159D1660232F3EE03E151B9
│   Symmetric algo: AES-256
│   Decryption successful
│
├── One-Pass Signature Packet, new CTB, 13 bytes
│       Version: 3
│       Type: Binary
│       Pk algo: RSA
│       Hash algo: SHA512
│       Issuer: 44FE231F3F3926E4
│       Last: true
│
├── Literal Data Packet, new CTB, 10 bytes
│       Format: Binary data
│       Content: "foo\n"
│
├── Signature Packet, new CTB, 658 bytes
│       Version: 4
│       Type: Binary
│       Pk algo: RSA
│       Hash algo: SHA512
│       Hashed area:
│         Signature creation time: 2026-02-14 09:01:10 UTC (critical)
│         Issuer: 44FE231F3F3926E4
│           Sergei Trofimovich <slyich@gmail.com> (authenticated)
│         Notation: salt@notations.sequoia-pgp.org
│           00000000  25 33 44 74 e7 b8 1a 28  1c b1 56 bd f0 02 4e 02
│           00000010  26 fe dd 1f c8 8c ab 11  9d 18 f3 7b bd 39 0c ad
│         Issuer Fingerprint: 62197C11C7C25A61C448E95644FE231F3F3926E4
│           Sergei Trofimovich <slyich@gmail.com> (authenticated)
│         Intended Recipient: 62197C11C7C25A61C448E95644FE231F3F3926E4
│       Digest prefix: F208
│       Level: 0 (signature over data)
│
└── Modification Detection Code Packet, new CTB, 20 bytes
        Digest: 1A14AA0FFDCB56E6BD64E005BA088EF591344F8D
        Computed digest: 1A14AA0FFDCB56E6BD64E005BA088EF591344F8D
        Valid: true
```

Here it's slightly more obvious that session key was encrypted with
`RSA`, data was encrypted with `AES-256` and was signed with `SHA-512`.

`sq key export` (private) and `sq cert export` (public) are a nice
complement to `sq inspect` and `sq packet dump` to get the idea what's
in the keys.

## Parting words

I found `sq` UI quite usable to rekindle some `PGP` interest in me. I
even managed to fix messed up list of revoked identities on the current
key.

I don't use anything advanced like smart cards to store keys or
detached offline certification key and I suspect `sq` has some
limitations there. But at least now I do understand what those things
are and how they are useful!

Have fun!
