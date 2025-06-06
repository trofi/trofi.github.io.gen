---
title: "Filing STABLEREQ for lazies"
date: March 23, 2020
---

## Tl;DR

If you are a lazy Gentoo package maintainer (like me) and don't file
stable requests too frequently because it's tedious I have a hack for
you!

```
$ pkgcheck scan --keywords=StableRequest --repo=gentoo dev-embedded/u-boot-tools | ./file_stablereq.py

dev-embedded/u-boot-tools
  StableRequest: version 2020.01: slot(0) no change in 49 days for unstable keywords: [ ~amd64, ~arm, ~x86 ]
Stablereq URL: https://bugs.gentoo.org/enter_bug.cgi?form_name=enter_bug&product=Gentoo+Linux&component=Stabilization&bug_status=CONFIRMED&bug_severity=normal&short_desc=%3Ddev-embedded%2Fu-boot-tools-2020.01+stabilization&comment=In+tree+for+49+days.+Let%27s+stabilize+%3Ddev-embedded%2Fu-boot-tools-2020.01+for%3A%0A++++amd64+arm+x86&keywords=STABLEREQ&assigned_to=embedded%40gentoo.org&cc=amd64%40gentoo.org%2Carm%40gentoo.org%2Cx86%40gentoo.org&cf_stabilisation_atoms=dev-embedded%2Fu-boot-tools-2020.01&cf_runtime_testing_required=No
```

And you are one click away from filing a stable request! Note: all the
essential fields are pre-filled:

- `bugzilla` component
- all maintainers are `CC`ed
- all current stable `ARCH`es are `CC`ed
- stabilization atom is pre-filled

The magic script is at
<https://github.com/trofi/gentoo-qa/blob/master/file_stablereq.py>
That's it! The script is just a tiny wrapper around urlencode.
You still might want to check for already open bugs against you
favourite package.

## More hacks

As I don't remember all the Gentoo packages I maintain or which
projects I am on I added a tiny fetcher of all packages I care about:
<https://github.com/trofi/gentoo-qa/blob/master/run_pkgcheck_for_maintained.sh>
Example uses go below.

Scan for all packages:

```
$ ./run_pkgcheck_for_maintained.sh

app-emulation/simh
  UnstableOnly: for arch: [ ppc ], all versions are unstable: [ 3.9.0-r1, 3.11.0 ]
  DeprecatedEapi: version 3.9.0-r1: uses deprecated EAPI 4
  RedundantVersion: version 3.9.0-r1: slot(0) keywords are overshadowed by version: 3.11.0
  PotentialStable: version 3.11.0: slot(0), stabled arches: [ amd64, x86 ], potential: [ ~ppc ]

app-editors/hteditor
  RedundantVersion: version 2.1.0: slot(0) keywords are overshadowed by version: 2.1.0-r1

app-emulation/ski
  UnstableOnly: for arches: [ amd64, x86 ], all versions are unstable: [ 1.3.2-r3, 1.3.2-r4 ]
  RedundantVersion: version 1.3.2-r3: slot(0) keywords are overshadowed by version: 1.3.2-r4
```

Get only package that might be good to stabilize by toolchain@:

```
$ PROJECTS=toolchain ./run_pkgcheck_for_maintained.sh --keywords=StableRequest


sys-libs/binutils-libs
  StableRequest: version 2.34: slot(0) no change in 51 days for unstable keywords: [ ~amd64, ~arm, ~arm64, ~hppa, ~ia64, ~m68k, ~ppc, ~ppc64, ~s390, ~sh, ~sparc, ~x86 ]

sys-kernel/linux-headers
  StableRequest: version 5.5: slot(0) no change in 54 days for unstable keywords: [ ~amd64, ~arm, ~arm64, ~hppa, ~ia64, ~m68k, ~ppc, ~ppc64, ~s390, ~sh, ~sparc, ~x86 ]

sys-devel/gcc
  StableRequest: version 5.5.0: slot(5.5.0) no change in 164 days for unstable keywords: [ ~amd64, ~arm, ~arm64, ~hppa, ~ia64, ~m68k, ~ppc, ~ppc64, ~s390, ~sh, ~sparc, ~x86 ]
  StableRequest: version 9.2.0-r3: slot(9.2.0) no change in 37 days for unstable keywords: [ ~amd64, ~arm, ~arm64, ~hppa, ~ia64, ~ppc, ~ppc64, ~s390, ~sparc, ~x86 ]
```

And generate clickable `STABLEREQ` URLs for them:

```
$ PROJECTS=toolchain ./run_pkgcheck_for_maintained.sh --keywords=StableRequest | ./file_stablereq.py

sys-libs/binutils-libs
  StableRequest: version 2.34: slot(0) no change in 51 days for unstable keywords: [ ~amd64, ~arm, ~arm64, ~hppa, ~ia64, ~m68k, ~ppc, ~ppc64, ~s390, ~sh, ~sparc, ~x86 ]
Stablereq URL: https://bugs.gentoo.org/enter_bug.cgi?form_name=enter_bug&product=Gentoo+Linux&component=Stabilization&bug_status=CONFIRMED&bug_severity=normal&short_desc=%3Dsys-libs%2Fbinutils-libs-2.34+stabilization&comment=In+tree+for+51+days.+Let%27s+stabilize+%3Dsys-libs%2Fbinutils-libs-2.34+for%3A%0A++++amd64+arm+arm64+hppa+ia64+m68k+ppc+ppc64+s390+sh+sparc+x86&keywords=STABLEREQ&assigned_to=toolchain%40gentoo.org&cc=amd64%40gentoo.org%2Carm%40gentoo.org%2Carm64%40gentoo.org%2Chppa%40gentoo.org%2Cia64%40gentoo.org%2Cm68k%40gentoo.org%2Cppc%40gentoo.org%2Cppc64%40gentoo.org%2Cs390%40gentoo.org%2Csh%40gentoo.org%2Csparc%40gentoo.org%2Cx86%40gentoo.org&cf_stabilisation_atoms=sys-libs%2Fbinutils-libs-2.34&cf_runtime_testing_required=No

sys-kernel/linux-headers
  StableRequest: version 5.5: slot(0) no change in 54 days for unstable keywords: [ ~amd64, ~arm, ~arm64, ~hppa, ~ia64, ~m68k, ~ppc, ~ppc64, ~s390, ~sh, ~sparc, ~x86 ]
Stablereq URL: https://bugs.gentoo.org/enter_bug.cgi?form_name=enter_bug&product=Gentoo+Linux&component=Stabilization&bug_status=CONFIRMED&bug_severity=normal&short_desc=%3Dsys-kernel%2Flinux-headers-5.5+stabilization&comment=In+tree+for+54+days.+Let%27s+stabilize+%3Dsys-kernel%2Flinux-headers-5.5+for%3A%0A++++amd64+arm+arm64+hppa+ia64+m68k+ppc+ppc64+s390+sh+sparc+x86&keywords=STABLEREQ&assigned_to=toolchain%40gentoo.org&cc=amd64%40gentoo.org%2Carm%40gentoo.org%2Carm64%40gentoo.org%2Chppa%40gentoo.org%2Cia64%40gentoo.org%2Cm68k%40gentoo.org%2Cppc%40gentoo.org%2Cppc64%40gentoo.org%2Cs390%40gentoo.org%2Csh%40gentoo.org%2Csparc%40gentoo.org%2Cx86%40gentoo.org&cf_stabilisation_atoms=sys-kernel%2Flinux-headers-5.5&cf_runtime_testing_required=No

sys-devel/gcc
  StableRequest: version 5.5.0: slot(5.5.0) no change in 164 days for unstable keywords: [ ~amd64, ~arm, ~arm64, ~hppa, ~ia64, ~m68k, ~ppc, ~ppc64, ~s390, ~sh, ~sparc, ~x86 ]
Stablereq URL: https://bugs.gentoo.org/enter_bug.cgi?form_name=enter_bug&product=Gentoo+Linux&component=Stabilization&bug_status=CONFIRMED&bug_severity=normal&short_desc=%3Dsys-devel%2Fgcc-5.5.0+stabilization&comment=In+tree+for+164+days.+Let%27s+stabilize+%3Dsys-devel%2Fgcc-5.5.0+for%3A%0A++++amd64+arm+arm64+hppa+ia64+m68k+ppc+ppc64+s390+sh+sparc+x86&keywords=STABLEREQ&assigned_to=toolchain%40gentoo.org&cc=amd64%40gentoo.org%2Carm%40gentoo.org%2Carm64%40gentoo.org%2Chppa%40gentoo.org%2Cia64%40gentoo.org%2Cm68k%40gentoo.org%2Cppc%40gentoo.org%2Cppc64%40gentoo.org%2Cs390%40gentoo.org%2Csh%40gentoo.org%2Csparc%40gentoo.org%2Cx86%40gentoo.org&cf_stabilisation_atoms=sys-devel%2Fgcc-5.5.0&cf_runtime_testing_required=No
  StableRequest: version 9.2.0-r3: slot(9.2.0) no change in 37 days for unstable keywords: [ ~amd64, ~arm, ~arm64, ~hppa, ~ia64, ~ppc, ~ppc64, ~s390, ~sparc, ~x86 ]
Stablereq URL: https://bugs.gentoo.org/enter_bug.cgi?form_name=enter_bug&product=Gentoo+Linux&component=Stabilization&bug_status=CONFIRMED&bug_severity=normal&short_desc=%3Dsys-devel%2Fgcc-9.2.0-r3+stabilization&comment=In+tree+for+37+days.+Let%27s+stabilize+%3Dsys-devel%2Fgcc-9.2.0-r3+for%3A%0A++++amd64+arm+arm64+hppa+ia64+ppc+ppc64+s390+sparc+x86&keywords=STABLEREQ&assigned_to=toolchain%40gentoo.org&cc=amd64%40gentoo.org%2Carm%40gentoo.org%2Carm64%40gentoo.org%2Chppa%40gentoo.org%2Cia64%40gentoo.org%2Cppc%40gentoo.org%2Cppc64%40gentoo.org%2Cs390%40gentoo.org%2Csparc%40gentoo.org%2Cx86%40gentoo.org&cf_stabilisation_atoms=sys-devel%2Fgcc-9.2.0-r3&cf_runtime_testing_required=No
```

Example bug created <https://bugs.gentoo.org/698646> from link above.
Note: the link just pre-populates all the boilerplate for you. You can
still type extra details into any of the fields.

## Canonical implementation

Almost every package maintainer has to deal with stabilization requests.
We don't have a simple tool to file them and have to come up with a
hack similar to the above.
It feels like `app-portage/gentoolkit` would be the perfect place for
such a tool. If you have more decent implementation (and you should!)
please consider posting a patch against `gentoolkit`.
Bug to track addition of canonical implementation:
<https://bugs.gentoo.org/708912>

## On filing `STABLEREQ`

Stabilization process is a basic `gentoo` vehicle to shield users of
stable packages from bleeding edge unstable world.

Did you know any user could file a stabilization request and not just
package maintainer? If you wonder why a package is sitting for years in
`~ARCH` maybe nobody just thought of stabilizing it. File a request!
See <https://wiki.gentoo.org/wiki/Stable_request> for a guide on what
usually goes there.

Have fun!
