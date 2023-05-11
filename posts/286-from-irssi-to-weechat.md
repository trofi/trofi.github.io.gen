---
title: "From irssi to weechat"
date: May 11, 2023
---

I started using `IRC` in 2003, around the same time I started using
`linux`. My IRC client of choice was `xchat`. I had a lot of fun
adapting an `xmms` plugin for `xchat` that pastes currently playing
song to the current chat room.

Then I switched to `irssi`. Oldest `irssi` logs I could find is from
2007. Around the time I was switching to Gentoo as my main desktop
system. I used `irssi` ever since. I had a few trivial plugins
installed: [nickcolor.pl](https://github.com/irssi/scripts.irssi.org/blob/master/scripts/nickcolor.pl)
and a few one-liners around `ChanServ` and `NickServ` integration.

A few weeks ago I noticed that `nickcolor.pl` plugin does not quite
work for some channels: when I open a chat tab for the first time (since
`irssi` startup) where the discussion is already ongoing all the
nicks get the same color. I spent some time debugging `nickcolor.pl`
and I did not manage to get it fixed. I rely on the colors a lot: I
frequent mix nicks together if they don't look distinct enough.

Instead of spending more time with `irssi` I decided to look at the
console alternatives that implement nick coloring by default and are
more actively maintained. [tiny](https://github.com/osa1/tiny) and
[weechat](https://weechat.org/) looked most promising.

`tiny` is written in `rust`, has a nice UI and has a bit too simplistic
`yaml` configuration for my needs. I also had an impression that
[CertFP](https://libera.chat/guides/certfp) support was added in
`master` branch only and did not see a proper release yet.

`weechat` is written in `C`, has UI very close to `irssi` and (to my
surprise) has even smaller amount of required dependencies than `irssi`.
It's configs are `ini` files with many-many options. `weechat` provides
excellent builtin support for searching through config options.
`weechat` also has a spell checking capability via `aspell`!

I was a bit worried about vulnerability count on `weechat`s side, but
apparently `irssi` has very similar profile if we look at the types of
encountered bugs:

- [weechat security page](https://weechat.org/doc/weechat/security/)
- [irssi security page](https://irssi.org/security/)

I settled on `weechat`.

## weechat config

Here is my full configuration so far:

```
/server add libera irc.libera.chat/6697 -ssl -autoconnect
  /set irc.server.libera.ssl on
  /set irc.server.libera.ssl_verify on
  /set irc.server.libera.ssl_cert %h/certs/libera.pem
  /set irc.server.libera.sasl_mechanism external

  /set irc.server.libera.nicks ...
  /set irc.server.libera.username ...
  /set irc.server.libera.realname ...
  /set irc.server.libera.autojoin ...

/server add oftc irc.oftc.net/6697 -ssl -autoconnect

  /set irc.server.oftc.ssl on
  /set irc.server.oftc.ssl_verify on
  /set irc.server.oftc.ssl_cert %h/certs/oftc.pem

  /set irc.server.oftc.nicks ...
  /set irc.server.oftc.username ...
  /set irc.server.oftc.realname ...
  /set irc.server.oftc.autojoin ...

# don't notify on joins/leaves
/set weechat.look.buffer_notify_default message

# spell checking
/set spell.check.enabled on
/set spell.check.real_time on
/set spell.check.default_dict "en,ru"

# don't merge server messages from different servers
/set irc.look.server_buffer independent

# don't trigger upgency properties on terminal to avoid focus change
/set trigger.trigger.beep.enabled off

# avoid "blue", too dark on my colorscheme
/set weechat.color.chat_nick_colors "cyan,magenta,green,brown,lightblue,default,lightcyan,lightmagenta,lightgreen"

/save
```

One special thing to note here is absence of
`/set irc.server.oftc.sasl_mechanism external` line if we compare `OFTC`
and `Libera` setups. Presence of `external` on `OFTC` causes connection
drops with message `irc: client capability: SASL not supported; irc: disconnected from server`.

## Parting words

After 16 years of `irssi` usage I don't feel much discomfort switching
to `weechat`. Most of the keybindings are the same. The UI has slightly
more details that `irssi`'s. It felt overwhelming at first. But once I
read the [quickstart guide](https://weechat.org/files/doc/stable/weechat_quickstart.en.html)
and [user guide](https://weechat.org/files/doc/stable/weechat_user.en.html)
it became natural.

I have not yet ported any of my trivial plugins but it does not look
complicated at a first glance.

Have fun!
