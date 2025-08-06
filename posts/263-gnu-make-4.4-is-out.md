---
title: "GNU make 4.4 is out!"
date: November 01, 2022
---

Yesterday `GNU make` project
[announced 4.4 release](https://lists.gnu.org/archive/html/bug-make/2022-10/msg00247.html)!

The release contains extensive list of new additions and backward
incompatible changes in various corner cases.

Multi-target rule change [mentioned before](/posts/260-make-multiple-targets.html)
will not break builds in `make-4.4`. Breaking change was postponed for
`make-4.5` release. `make-4.4` will print diagnostic warning for simple
cases, Worth watching for the warnings. So far I see similar warnings in
the following packages:

```
dtc-1.6.1.drv:Makefile:387: warning: pattern recipe did not update peer target 'dtc-parser.output'.
ghc-9.0.2.drv:libraries/base/ghc.mk:4: warning: pattern recipe did not update peer target 'libraries/base/dist-install/build/GHC/Clock_hsc.c'.
git-2.38.1.drv:Makefile:354: warning: pattern recipe did not update peer target 'git.5'.
kmod-30.drv:Makefile:615: warning: pattern recipe did not update peer target 'depmod.5'.
libbonobo-2.32.1.drv:Makefile:802: warning: pattern recipe did not update peer target 'Bonobo_ActivationContext-imodule.c'.
ORBit2-2.14.19.drv:Makefile:1097: warning: pattern recipe did not update peer target 'test1-imodule.c'.
pciutils-3.8.0.drv:Makefile:114: warning: pattern recipe did not update peer target 'lspci.5'.
perf-linux-6.0.2.drv:Makefile:262: warning: pattern recipe did not update peer target 'perf.5'.
rethinkdb-2.4.1.drv:mk/support/build.mk:174: warning: pattern recipe did not update peer target 'install-include-gtest_1.7.0'.
sane-backends-1.0.32.drv:Makefile:957: warning: pattern recipe did not update peer target 'gamma4scanimage.5'.
shadow-4.11.1.drv:Makefile:1062: warning: pattern recipe did not update peer target 'man1/chgpasswd.8'.
```

Worth fixing those upstream before next `make-4.5` release.

On another topic `make --shuffle`
[mentioned before](/posts/249-an-update-on-make-shuffle.html) is now
available in official release. This should lower the barrier for users
to try it out once `make-4.4` trickles into downstream distributions.

Have fun!
