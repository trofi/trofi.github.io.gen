---
title: "A year on NixOS"
date: September 04, 2022
---

Discourse told me that one year has passed since I switched over to
`NixOS` for my main desktop system. It feels like it was yesterday.
Time to write a bit of retrospective of past 6 (or so) months.

## System maintenance

Nightly `nixos-unstable` automatic upgrades are surprisingly fast and
smooth. I had one period when I was not able to update
[claws-mail](https://github.com/NixOS/nixpkgs/pull/185988)
for Perl-related build failures. I ended up doing nothing for a week and
somebody fixed it for me! My system was failing updates all that week.
It did not prevent me in any way from doing my usual activities.

I had to tweak `/etc/nixos/` config 1 time to adapt to option rename.
The warning message told me new name of the field: `nix.systemFeatures`
-> `nix.settings.system-features`. That was smooth as well.

I don't remember any other maintenance-related activities I had to do
to make the desktop working. I reeboted 2-3 times each month for
various unrelated reasons (power loss, multi-day travel, etc.). Reboots
exposed me to very fresh kernel versions each time. I had no troubles
with them either.

## Unusual software

Of slightly unusual deeds I installed 32-bit wine with `pipewire` socket
passed to a sandboxed used from my main user. That went very smooth and
effortless as well.

Otherwise I'm afraid I don't need anything special from the
distribution. Most packages I need are widely used and the rare ones
missing are trivial to package in a personal overlay. Just yesterday
I added a trivial [uselex.rb](https://github.com/trofi/uselex) expression:

```nix
{ lib
, stdenv
, fetchFromGitHub

, ruby_3_1
, binutils-unwrapped-all-targets

, unstableGitUpdater
}:

stdenv.mkDerivation rec {
  pname = "uselex";
  version = "unstable-2017-07-28";

  src = fetchFromGitHub {
    owner = "trofi";
    repo = "uselex";
    rev = "dfddc3dc839500edceca4665af7ee38f90e92081";
    sha256 = "sha256-4/HK+E1iiwMIvDwo+IheA+tETref9tCAG6WcB34CbKE=";
  };

  buildInputs = [ ruby_3_1 ];

  postPatch = ''
    patchShebangs

    substituteInPlace uselex.rb --replace "'nm'" "'${binutils-unwrapped-all-targets}/bin/nm'"
  '';

  installPhase = ''
    install -d $out/bin
    install -m 0755 uselex.rb $out/bin
  '';

  # Update as:
  #    nix-shell ./maintainers/scripts/update.nix --argstr package uselex --arg include-overlays true
  passthru.updateScript = unstableGitUpdater {
    url = "https://github.com/trofi/uselex";
  };

  meta = with lib; {
    description = "Look for USEless EXports in object files.";
    homepage = "https://github.com/trofi/uselex";
    license = licenses.publicDomain;
    maintainers = with maintainers; [ trofi ];
    platforms = platforms.all;
  };
}
```

## Community support

I keep being amazed by inclusivity and depth of technical expertise of
`NixOS` community. Maybe it's just because I did not break anything
major yet? :)

I almost never see any blame or aggresive behaviour. And when rare
incidents happen people do step in and explain why it's not OK and
suggest various ways out of a conflict. That makes the community a great
collaborative environment. At least from the far viewpoint I interact
with it.

People have the courage to develop things I would never dare
to tackle alone (but I would be happy to help with small improvements).
One example is cross-compilation of huge chunk of `nixpkgs`
([mc example](https://github.com/MidnightCommander/mc/commit/6b67d231a2f447cf5f33180c618c2a67849e6d15)).

I keep learning new details about `nixpkgs` and other software. It's
so fun!

I don't have the time to follow all the `NixOS Dev` matrix chat I'm
in. It's just too many updates usually. But the other ones are low
traffic enough. My favourites are probably `Nix Cross Compiling`
(where the main topic is toolchains and related issues) and `Staging`
(where I mostly track merges of `staging-next` into `master` out of
idle curiosity).

I do have time to follow relatively low traffic
<https://discourse.nixos.org/> and occasionally post announcements there
myself.

## Maintenance model

At first I thought that `nixpkgs` maintenance model is too simple to be
viable for a large package base: most package updates are not made by
(or even approved by) package maintainers. It felt a bit fragile: after
all there is always a chance of some fancy detail about the package that
needs to be kept in mind.

At least the partial answer to that is that `nixpkgs` has quite a bit of
tests. If new `nixpkgs` can boot my desktop in `qemu` after a `binutils`
update it does not really matter if there are minor mistakes in it. We
can always revise it and fix later.

Most trivial package updates are done by ... an automated system! Just
look at [these stats](https://github.com/NixOS/nixpkgs/graphs/contributors).
It is so surreal to allow the bot file update PRs. But on the other hand
for smaller packages it's exactly what humans would have to do anyways.
Why not pre-populate the PR with mechanical details already sorted?
Reviewers (and better yet automated tests) can always object to special
cases of more complex packages.

Having finished recently [binutils-2.39 update](https://github.com/NixOS/nixpkgs/pull/185297)
I would say maintainership works great. I hope we will break no packages
by that somewhat disruptive (API change wise) update.

## Home server experience

I don't have anything to complain about. Over past 6 months I played a
bit with distributed compilation which worked great. Otherwise I did not
do any major configuration changes on the server. It still serves `HTTP`
and a few services. Required zero maintenance from me over that period.

Automatic reboots on kernel upgrade makes server's uptime lebow 2-3 days
on average. It does not feel disruptive and adds a bit of peace of mind
against exploits against outdated software.

## Bugs? What bugs?

`NixOS`-specific bugs do certainly happen time to time.

Mostly they stem from the fact that default paths are unusual (usually
easy to deal with) and split across multipl locations (sometimes
requires patching or clever symlink hacks).

Past examples are [libtool](https://github.com/NixOS/nixpkgs/pull/187694)
embedding `/nix/sotore` paths to `make dist` tarballs and non-working
[include-what-you-use](https://github.com/NixOS/nixpkgs/issues/189753) due
to dynamic headers location.

## Parting words

I think `NixOS` works well for me so far. If you still did not consider
trying it as well :)
