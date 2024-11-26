---
title: log
---

These are notable or amusing bugs and patches I dealt with in the past.

## 2024

26 Nov: `nix` bug: [`bz2` `meson` detection failure](https://github.com/NixOS/nix/pull/11974).

6 Nov: `pixman` bug: [`rvv` `riscv64` extension is not fully detected](https://gitlab.freedesktop.org/pixman/pixman/-/merge_requests/128).

31 Oct: `swtpm` bug: [32-bit file API use on filesystem with 64-bit inodes](https://github.com/stefanberger/swtpm/pull/941).

24 Oct: `gcc` bug: [`-flto` fails on array initializers](https://gcc.gnu.org/PR117288).

28 Oct: `unbound` bug: [`make --shuffle` found multiple targets rule error](https://github.com/NLnetLabs/unbound/pull/1167).

13 Oct: `gcc` bug: [false positive warnings on overrides for multiple inheritance](https://gcc.gnu.org/PR117114).

29 Sep: `gcc` bug: [coroutine waiter destroyed wait object too early](https://gcc.gnu.org/PR116880).

18 Sep: `nixpkgs` bug: [`/etc/wpa_supplicant.conf` is zeroed out](https://github.com/NixOS/nixpkgs/pull/180872#issuecomment-2359355734).

6 Sep: `nix` bug: [`toml` parser changes timestamp parsing/printing rules](https://github.com/NixOS/nix/issues/11441).

24 Aug: `warzone` bug: [use-after-free detected by `gcc` warning](https://github.com/Warzone2100/warzone2100/pull/4051).

24 Aug: `diffoscope` bug: [crash when attempt to diff a symlink to the directory](https://salsa.debian.org/reproducible-builds/diffoscope/-/merge_requests/144).

29 July: `less` bug: [`PageUp` / `PageDown` keys don't work anymore](https://github.com/gwsw/less/issues/549).

16 July: `gcc` bug: [saturated truncation works incorrectly on bitfields](https://gcc.gnu.org/PR115961).

4 June: `nixpkgs`: [`stdenv` fails on `sourceRoot` with dashes](https://github.com/NixOS/nixpkgs/pull/314683).

1 June: `nixpkgs`: [`gcc.libgcc` on `musl` is not stripped](https://github.com/NixOS/nixpkgs/pull/316334).

14 April: `linux` bug: [`eevdf` scheduler sometimes crashes kernel](https://lkml.org/lkml/2024/4/14/134).

1 April: `nixpkgs`: [fix `pcre2` to enable `jit`](https://github.com/NixOS/nixpkgs/pull/300565).

14 Mar: `gentoo` bug: [`eautoreconf` does not handle `AC_CONF_AUX_DIR`](https://bugs.gentoo.org/927017).

9 Mar: `nix` bug: [`nix` does not handle root flakes](https://github.com/NixOS/nix/issues/10202).

16 February: `waf` bug: [`i686-linux` does not fit into 3GB](https://github.com/NixOS/nixpkgs/pull/289405).

30 January: `binutils` bug: [`.arch i386` is no accepted by `gas` anymore](https://sourceware.org/PR31319).

28 January: `nixpkgs`: [document bootstrap seed update procedure](https://github.com/NixOS/nixpkgs/pull/284541).

13 January: `mikmod`: ["buffer overflow" detected at startup](https://sourceforge.net/p/mikmod/patches/17/).

13 January: `nixpkgs`: [`autoconf: 2.71 -> 2.72` update](https://github.com/NixOS/nixpkgs/pull/276343).

## 2023

26 December: `ofborg`: [map modified files according to `.meta.position`](https://github.com/NixOS/ofborg/pull/669).

16 December: `iwd`: [fix data corruption in `aarch64`](https://git.kernel.org/pub/scm/network/wireless/iwd.git/commit/?id=688d27700833258a139a6fbd5661334bd2c9fa98).

8 December: `libbpf`: [report nature of `ELF` section corruption](https://git.kernel.org/pub/scm/linux/kernel/git/bpf/bpf-next.git/commit/?id=32fa058398624166dd04ff4af49cfef69c94abbc).

3 December: `tracker`: [`tracker` used invalid code to test `strftime()` features](https://gitlab.gnome.org/GNOME/tracker/-/merge_requests/638).

1 December: `nixpkgs` bug: [`nixpkgs` uses very old `libelf` library where one would expect `elfutils`](https://github.com/NixOS/nixpkgs/issues/271473).

28 November: `php`: [Fix build on `gcc-14` `C11` atomics](https://github.com/php/php-src/pull/12821).

14 November: `libglgnd`: [Enable 64-bit file APIs](https://gitlab.freedesktop.org/glvnd/libglvnd/-/merge_requests/288).

11 November: `nixpkgs`: [Fix broken `SUID` binary in `pam` package](https://github.com/NixOS/nixpkgs/pull/266828).

10 November: `duperemove`: [Fix quadratic slowdown for `partial` mode](https://github.com/markfasheh/duperemove/pull/324).

9 November: `duperemove`: [Fix file sharing accounting](https://github.com/markfasheh/duperemove/pull/323).

7 November: `duperemove`: [Fix quadratic slowdown of small inline files](https://github.com/markfasheh/duperemove/pull/322).

5 November: `btrfs-progs`: [Fix accidental SCAN mapped to FORGET `ioctl`](https://github.com/kdave/btrfs-progs/pull/706).

4 November: `nixpkgs`: [one-liner to reduce meld closure](https://github.com/NixOS/nixpkgs/pull/265454).

1 November: `duperemove`: [one-liner to speed deduping up 2x](https://github.com/markfasheh/duperemove/pull/318).

31 October: `duperemove` bug: [hang up on 1 million of small files](https://github.com/markfasheh/duperemove/issues/316).

23 October: `gcc`: [`libgcc`: make heap-based trampolines conditional on `libc` presence](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=eaf75155f38a51b2d9d49c6c1b1e1639e3d0591a).

20 October: `acme-rs` [Fix non-standalone challenge paths](https://github.com/kariustobias/acme-rs/pull/29).

28 September: `gcc`: [`ggc`: do not wipe out unrelated data via `gt_ggc_rtab`](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=7525707c5f3edb46958c4fdfbe30de5ddfa8923a).

27 September: `gcc`: [`ipa-utils`: avoid uninitialized probabilities on `ICF`](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=043a6fcbc27f8721301eb2f72a7839f54f393003).

16 September: `nixpkgs`: [disable `bash` `malloc()` implementation](https://github.com/NixOS/nixpkgs/commit/223b83ca54ac118f7769694b09af3547d5c39224).

14 September: `nixpkgs`: [always mangle out `__FILE__` paths to shrink closure](https://github.com/NixOS/nixpkgs/pull/255192).

31 July: `nixpkgs`: [fix file corruption caused by parallel strip](https://github.com/NixOS/nixpkgs/commit/a3d2e71a6c6fbf389c19940899440228591b7b92).

15 March: `nixpkgs`: [enable parallel installs for pa rallel builds](https://github.com/NixOS/nixpkgs/commit/19680e9902c2f8607022189c8583c9d697969fa1).

20 February: `libomxil-bellagio`: [fix stack overread](https://sourceforge.net/p/omxil/patches/8/).

5 February: `xmms2`: [fix `UPD` visualization server on dual-stack `IPv6`](https://github.com/xmms2/xmms2-devel/commit/d9d64c1d303593205251494ea4c8847174dd5e9b).

11 January: `nix-olde`: [started a new project to find outdated `nixpkgs` packages](https://github.com/trofi/nix-olde).

## 2022

30 December: `xmms2`: [fix stuck `FLAC` playback](https://github.com/xmms2/xmms2-devel/commit/39d31d4a7ae463f3df7a09915fe61e2574f4d95f).

1 December: `AoC`: [started participating in `Advent of Code`](https://github.com/trofi/AoC).
Turned it into `rust` learning exercise.

13 September: `glibc`: [fix `MAKEFLAGS` assignment for upcoming `make-4.4`](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=2d7ed98add14f75041499ac189696c9bd3d757fe).

11 September: `make`: [fix `--shuffle` crash on `SECONDEXPANSION` prerequisites](https://git.savannah.gnu.org/cgit/make.git/commit/?id=ca4234c4b550618df2194e0617c43bb12524f820).

16 August: `gcc`: [add build dependencies on `i386-builtin-types.inc`](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=ef3165736d9daafba88adb2db65b2e8ebf0024ca).
Exposed by `make --shuffle`.

16 August: `gcc`: [driver: fix environ corruption after `putenv()`](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=2b403297b111c990c331b5bbb6165b061ad2259b).

11 August: `nixpkgs`: [don't inhibit stripping when `debuginfo` splitter is enabled](https://github.com/NixOS/nixpkgs/pull/185537).

28 July: `nixpkgs`: [enable stripping of cross-compilers](https://github.com/NixOS/nixpkgs/commit/88c63ca65a30eb83bd2a00d183a9f11819be43ad).

27 June: `gcc`: [`c++`: avoid `<memory>` poisoning on `musl`](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=3b21c21f3f5726823e19728fdd1571a14aiae0fb3).
25 June: `ghc`: [add missing order-only `.hs-boot` dependencies](https://gitlab.haskell.org/ghc/ghc/-/commit/b43d140b3f79e024489bbd9338d81d2ac23fc437).

4 June: `make`: [add `--shuffle` mode](https://git.savannah.gnu.org/cgit/make.git/commit/?id=621d3196fae94e9006a7e9c5ffdaf5ec209bf832).

4 June: `nixpkgs` bug: [`unzip` unpacked different files on `linux`/`darwin`](https://github.com/NixOS/nixpkgs/issues/176225).

23 May: `glibc`: [fix build against ancient `gcc-4.7`](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=5a5f94af0542f9a35aaa7992c18eb4e2403a29b9).

15 April: `gcc`: [`gcov-profile`: Allow negative counts of indirect calls](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=90a29845bfe7d6002e6c2fd49a97820b00fbc4a3).

14 April: `binutils`: `m68k` [fix quadratic slowdown of label alignment check](https://sourceware.org/git/?p=binutils-gdb.git;a=commitdiff;h=c641fe0dcb886dc1b8a235ab2b236275ee46510a).

7 April: `gcc`: `ia64` [`libgcc`: don't compile glibc-based unwinder without libc headers](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=cef03728234644b1a9ad5bd8e94daeac8650a71b).

22 February: `nixpkgs`: [fix `crt` file poisoning in bootstrap](https://github.com/NixOS/nixpkgs/commit/86be4335afe7ce177a8e3c0b0293e1652d4ce682).

20 March: `vim`: [fix tutor parallel install](https://github.com/vim/vim/pull/9978).

12 March: `gcc`: [avoid `TEXTREL`s in shared library](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=638e630142b2cf691db890ab9eb6d77a65339c54).

17 March: `nix`: [fix `nix store gc` quadratic behaviour](https://github.com/NixOS/nix/commit/d58453f72ea584cac2e3362fd6a73fcf0e3b615e).

13 March: `nix`: [fix `nix store gc` optimized store](https://github.com/NixOS/nix/commit/6b1872312f1b505334acb67b8bf7990b0a0fdbd8).

13 January: `rustc` bug: [`lto = "thin"` causes bad code generation](https://github.com/rust-lang/rust/issues/92869).

## 2021

21 November: `flatbuffers`: [fix undefined evaluation order of function parameters](https://github.com/google/flatbuffers/pull/6946).

10 November: `nix`: [fix quadratic slowdown of lookup and deletion of realizations for CA derivations](https://github.com/NixOS/nix/commit/edfc5b2f127bfbaebbd48fcd7b35034345ce2cfa).

7 November: `espeak-ng`: [fix `SIGSEGV` on parallel builds](https://github.com/espeak-ng/espeak-ng/pull/1036).
Build rules shared temporary file names.

9 September: `nixpkgs`: [add `s390x` cross-compilation target](https://github.com/NixOS/nixpkgs/commit/34e468dc4268cee86aa019ae9bc52768e60fb5f7).

21 August: `guix`: [fix `guix pull` hang up due to `po4a` bug](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=79c07db3d52fb97f38de13d409264c5194e132fe).

6 August `gcc`: [`c++`: fix `ptrmemfunc` template instantiation](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=27a1fb385b7fe706f05608e53f3e91d7d3442b8b).

23 July: `linux`: `mm` [fix `page_poison=1` data corruption](https://github.com/torvalds/linux/commit/69e5d322a2fb86173fde8bad26e8eb38cad1b1e9).
Without the change freeing a page always triggered assertions.

27 June: `libffi`: [`configure.ac`: add `--disable-exec-static-tramp` flag](https://github.com/libffi/libffi/commit/132699b95d3ee4d889ea2a80540acf3300987dad).

13 June: `ccache` bug: [`PWD=.` seems to trigger asserts in `ccache`](https://github.com/ccache/ccache/issues/860).

29 April: `linux`: `mm` [fix data corruption caused by `debug_pagealloc=1`](https://github.com/torvalds/linux/commit/9df65f522536719682bccd24245ff94db956256c).
Without the fix zero-initialized pages were returned as `0xaa`-initialized.

29 April: `linux`: `mm` [fix endless recursion tracking `page_owner`](https://github.com/torvalds/linux/commit/8e9b16c47680f6e7d6e5864a37f313f905a91cf5).

29 April: `linux`: `ia64` [fix symbolizer crash](https://github.com/torvalds/linux/commit/99e729bd40fb3272fa4b0140839d5e957b58588a).

2 April: `linux`: `ia64` [fix `ptrace()` stack pointer fetch](https://github.com/torvalds/linux/commit/7ad1e366167837daeb93d0bacb57dee820b0b898).

30 March: `linux`: `hpsa` [fix `ia64` boot failure for unaligned SCSI commands](https://github.com/torvalds/linux/commit/02ec144292bc424a5800d45d4cb472c66e97c520).

12 March: `linux`: `ia64` [fix `ptrace(PTRACE_SYSCALL_INFO_EXIT)` sign](https://github.com/torvalds/linux/commit/61bf318eac2c13356f7bd1c6a05421ef504ccc8a).

12 March: `linux`: `ia64` [fix tracing of break-based syscalls](https://github.com/torvalds/linux/commit/0ceb1ace4a2778e34a5414e5349712ae4dc41d85).

5 February: `glibc`: [`nsswitch`: return result when `nss` database is locked](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=c3479fb7939898ec22c655c383454d6e8b982a67).

3 February: `mc`: [fix shadow render crash](https://github.com/MidnightCommander/mc/commit/6394547dbffbad44ea50c64c282de4b610ca07bf).

11 January: `gcc`: [fix `modref` analysis on `RVO` statements](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=11056ab7687f7156846e93557c9171b77713bd7e).

## 2020

6 September: `gcc`: [`gcov`: fix `TOPN` streaming from shared libraries](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=4ecf368f4b4223fb2df4f3887429dfbb48852e38).

25 July: `gcc`: [don't crash on unoptimized lto functions](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=cbf10ac51c0b889e930f260a3d1fb601332befdf).

21 July: wrote <https://wiki.gentoo.org/wiki/Stack_smashing_debugging_guide>
article to aid in stack smashing debugging.

14 July: `gcc`: `sparc` [fix `PIE` destructors](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=87891d5eafe8d1de90b9d9b056eca81c508d1c77).

11 July: `glibc`: `ia64` [fix build against `-fno-common` toolchain](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=073edbdfabaad4786e974a451efe4b6b3f7a5a61).

31 May: `proftpd`: [fix `SIGSEGV` on `pstrcat()` call without a sentinel `NULL`](https://github.com/proftpd/proftpd/commit/6ac1c727ddfd70080b38097e5484390ec84ef9be).
And a follow up [change](https://github.com/proftpd/proftpd/commit/c5f98b6e047e0e5ca841372d78d06c05fe8770c6)
to turn such bugs into compile-time errors.

21 April: `ccache` bug: [`ccache` does not handle directories in `-fprofile-use=`](https://github.com/ccache/ccache/issues/582).

15 April: `qemu`: `alpha` target fix [for `epoll_create()` emulation](https://gitlab.com/qemu-project/qemu/-/commit/386d38656889a40d29b514ee6f34997ca18f741e).

16 March: `ccache` bug: [another attempt by `ccache` to delete `/dev/null`](https://github.com/ccache/ccache/issues/564).

18 January: `seekwatcher`: [forked and fixed minor bugs in a project by Chris Mason](https://github.com/trofi/seekwatcher).

3 January: `binutils`: `ia64` [fix for `binary` `ld` merge](https://sourceware.org/git/?p=binutils-gdb.git;a=commitdiff;h=b26a3d5827edcb942b3af5b921bf317bbc0c8e83).

## 2019

25 November: `nix-guix-gentoo`: [moved `nix` and `guix` `ebuilds` to an external overlay](https://github.com/trofi/nix-guix-gentoo).

28 April: `ccache` bug: [`ccache` tried to rename `/dev/null`](https://github.com/ccache/ccache/issues/397).

27 April: `libffi`: `hppa` [avoid `TEXTREL` in .eh_frame section](https://github.com/libffi/libffi/commit/fadf1eb530713fde0be9774d926bc8202c97e379).

20 March: `ghc`: [fix 64-bit comparisons on `ppc32`](https://gitlab.haskell.org/ghc/ghc/-/commit/973077ac63c79988f2d5f25d13b60dce82f9e8dd).

10 March: `linux`: `tty/vt` race condition crash [fix write/write race in `ioctl(KDSKBSENT)` handler](https://github.com/torvalds/linux/commit/46ca3f735f345c9d87383dd3a09fa5d43870770e)

## 2018

31 Dec: `linux`: `alpha` [fix page fault handling for `r16`-`r18` registers](https://github.com/torvalds/linux/commit/491af60ffb848b59e82f7c9145833222e0bf27a5).
Without the change page fault handler corrupted wrong register.

21 September: `ski`: [started the fork of `HP`'s `ski` to adapt to modern systems](https://github.com/trofi/ski).

3 August: `gcc`: `sh` [avoid `TEXTRELs` in `PIC` code](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=f6a9dfd3699430b40febb4d4a894e5a9e6302874).

16 July: `ghc`: [skip `-Bsymbolic` on unregisterised targets](https://gitlab.haskell.org/ghc/ghc/-/commit/8ec48990fee9e245bb2fe40dc6f65b61b8612157).
This fixed `sh4` build on `ghc`.

19 May: `libressl`: `hppa` [fix assembly syntax around semicolons](https://github.com/libressl/portable/pull/426).

16 May: `goircbot`: [set bot status before channel joins, not after](https://github.com/StalkR/goircbot/pull/18).

17 February: `libffi`: `ia64` [fix small struct return](https://github.com/libffi/libffi/commit/b58caef7fd620408be9239ac24ea89d5bc84f30b).

11 February: `libffi`: `ia64` [fix variadic function closures with FP arguments](https://github.com/libffi/libffi/commit/11de69ddb788e4d87ef653898878384116ac16c6).

## 2017

21 December: `glibc`: `m68k` [fix clobbering `a5` in `setjmp()`](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=6eb7e1da0e805e2893a0b70a5813641529d8c7e2).

18 December: `glibc`: `mips32` [fix clobbering `s0` in `setjmp()`](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=c85c564d1442f0bc09a6c80fca025f004e12d044).

18 December: `glibc`: `mips64` [fix clobbering `s0` in `setjmp()`](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=368b6c8da9f8ae453f5d70f8a62dbf3f1b6d5995).

17 October: `libffi`: `ia64` [unbreak small struct handling](https://github.com/libffi/libffi/commit/ed7488c003765c7ee71b7da3e8c21f431b043809).

3 October: `fish-shell`: [fix `printf "%o"` on big-endian systems](https://github.com/fish-shell/fish-shell/pull/4444).

19 August: `pax-utils`: [fixed crash on `ia64` binaries](https://gitweb.gentoo.org/proj/pax-utils.git/commit/?id=720becce1314db8c0af8442650f496d972475327).

18 July: found `nettle` data corruption on `sparc` due to reuse of local
variable. It was fixed upstream with [this commit](https://git.lysator.liu.se/nettle/nettle/-/commit/dcda81d796de2f4a16fd7e9e7a5d07baa288f147).

14 July: `libunwind`: `ia64` [disable unimplemented `dwarf` support](https://github.com/libunwind/libunwind/pull/35).

10 July: `ruby`: `ia64`: [fix garbage collector scanning of `RBS` area](https://github.com/ruby/ruby/commit/7aa74a0d0a22d74f1b3053280c56501aaf0b1e3c).

24 June: `binutils`: `ia64` [fix for `strip` crash with broken hint](https://sourceware.org/git/?p=binutils-gdb.git;a=commitdiff;h=5cc4ca837deac7dc962d8a3741aa120c50ab41da).

30 May: `openrc`: [fix buffer overflow in `openrc-init`](https://github.com/OpenRC/openrc/commit/0ddee9b7d2b8dea810e252ca6a95c457876df120).

1 May: `linux`: `ia64` [fix module loading on `gcc-5.4+`](https://github.com/torvalds/linux/commit/a25fb8508c1b80dce742dbeaa4d75a1e9f2c5617).

24 April: `ghc`: [put const objects into `.rodata`](https://gitlab.haskell.org/ghc/ghc/-/commit/b68697e579d38ca29c2b84377dc2affa04659a28).

## 2016

2 September: `ghc`: [extend worker/wrapper limit to specializers](https://gitlab.haskell.org/ghc/ghc/-/commit/f93c363fab1ac8ce6f0b474f5967b0b097995827).

1 September: `ghc`: [restore limit on worker/wrapper transform](https://gitlab.haskell.org/ghc/ghc/-/commit/a48de37dcca98e7d477040b0ed298bcd1b3ab303).
Without the change `ghc` spent a lot of time to generate large slow functions.

30 August: `ghc`: [enable parallel GC scan for large nurseries](https://gitlab.haskell.org/ghc/ghc/-/commit/a5d26f26d33bc04f31eaff50b7d633444192b4cb).

10 March: `ghc`: [fix Float and Double cross-compilation](https://gitlab.haskell.org/ghc/ghc/-/commit/c42cdb7f6dcfd519d9607ac9fa53f049b2922fb8).

6 March: `ghc`: [fix closure alignment on `m68k`](https://gitlab.haskell.org/ghc/ghc/-/commit/ade1a461ab4ba3e6de3c4afe9fe9766b7b4e51b3).
Without it garbage collector crashed with assertion failures.

6 March: `qemu` bug: `m68k` [generated invalid `TCG`](https://github.com/vivier/qemu-m68k/issues/6).

5 January: `gcc`: `ia64` [don't use dynamic relocations for local symbols](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=face88a110a3c1970119ba5fea9679609242975c).

## 2015

7 August: `mtpfs`: [open `device=0`, not a random one](https://github.com/cjd/mtpfs/pull/4).

7 July: `ghc`: [fix wake up on bad file descriptors in `select()` backend](https://gitlab.haskell.org/ghc/ghc/-/commit/5857e0afb5823987e84e6d3dd8d0b269b7546166).

23 June: `ghc`: [fix `PLT` relocation support on `powerpc` `ghci`](https://gitlab.haskell.org/ghc/ghc/-/commit/c0847967caf51ea4ca88d0ffc25fe1bd99dcabed).

## 2014

14 December: `ghc`: [fix shared library support on `ppc`](https://gitlab.haskell.org/ghc/ghc/-/commit/fa31e8f4a0f853848d96549a429083941877bf8d).

4 September: `ghc`: [fix code/data pointer type confusion in CMM](https://gitlab.haskell.org/ghc/ghc/-/commit/e18525fae273f4c1ad8d6cbe1dea4fc074cac721).
Without the change on `ia64` function pointers were incorrect due to
missing function descriptor indirection.

26 August: `ghc`: [fix large literal emission for unregisterised builds](https://gitlab.haskell.org/ghc/ghc/-/commit/43f1b2ecd1960fa7377cf55a2b97c66059a701ef).

28 July: `ghc`: [fix `openTempFiles` linear slowdown](https://gitlab.haskell.org/ghc/ghc/-/commit/f510c7cac5b2e9afe0ebde2766a671c59137f3cc)
due to predictable temp files.

23 May: `ghc`: [fix CPU hangs of threaded runtime on bad file descriptors](https://gitlab.haskell.org/ghc/ghc/-/commit/9fd507e5758f4141ac2619f0db57136bcab035c6).
This fixed battery drain by `xmobar` on my laptop.

19 April: `dars`: [corrupted fetcher due to temp file collision](https://bugs.darcs.net/issue2364).

7 April: `linux`: [fix crash on thread pool remount](https://github.com/torvalds/linux/commit/800ee2247f483b6d05ed47ef3bbc90b56451746c).

4 March: `lambdabot`: [add `CODEPAGE` command](https://github.com/lambdabot/lambdabot/pull/76).

## 2013

13 December: `cabal`: [use `ByteString` instead of `String`](https://github.com/haskell/cabal/pull/1614).
This allowed me to upload `raincat` release on `hackage`.

25 November: `trofi.github.io`: [moved the blog to `GitHub pages`](https://github.com/trofi/trofi.github.io.gen).

3 July: `uselex`: started [new `uselex` project](https://github.com/trofi/uselex)
and unused code in many projects with it.

13 September: `linux`: [enable `-Werror=implicit-int` by default](https://github.com/torvalds/linux/commit/80970472179a45609c0b11b80619bc8c32b15f77).

23 January: `ghc`: [fix cross-compilation of `integer-gmp`](https://gitlab.haskell.org/ghc/ghc/-/commit/52f554582339d14c28a3cc91385f9cb0343f6779).
Instead of runtime-detection of structure layout we now use `autoconf` macros.

## 2012

31 October: `bdelta`: [25% speed up of `--all-in-ram` option](https://github.com/jjwhitney/BDelta/pull/2).

29 October: `xmms2`: [fix infinite loop on broken `.cue` files](https://github.com/xmms2/xmms2-devel/commit/ef1dd4afa356519874de3ac610e03635150c8a08).

26 September: `bdelta`: [add `--all-in-ram` option to get 3x speed up](https://github.com/jjwhitney/BDelta/pull/1).

24 April: `linux`: `btrfs` [feature to change thread pool sizes](https://github.com/torvalds/linux/commit/0d2450abfa359ff94a2bee64a7daeba68c346c81).

16 April: `linux`: `btrfs` [fix for remount data corruption](https://github.com/torvalds/linux/commit/8a3db1849e9e2563727ea2dc32737502e0096641).
Without the fix `btrfs` always triggered restore code if
`mount -oremount` fails for any reason.

## 2011

16 October: `chua`: [started a toy strange attractor render](https://github.com/trofi/chua).
My favourite `OpenGL` app. This allowed me to pass the exam in uni :)

25 August: `linux`: `alpha` [fix for `osf_setsysinfo()`](https://github.com/torvalds/linux/commit/2df7a7d1cd07626dd235ca102830ebfc6c01a09e)
obscure interface.

20 May: `linux`: `btrfs` [performance fix](https://github.com/torvalds/linux/commit/c4f675cd40d955d539180506c09515c90169b15b)
caused by spinning shrinker without a chance of progress.

11 April: `linux`: `btrfs` [data corruption fix](https://github.com/torvalds/linux/commit/3387206f26e1b48703e810175b98611a4fd8e8ea)
caused by use of `memcpy()` on overlapping areas.

## 2010

29 August: `ghc`: [fix `ppc` crashes on `foreign import wrapper`](https://gitlab.haskell.org/ghc/ghc/-/commit/33653031263aa6d5ba4d481c8bb0d8eb1303f4d1).

9 July: `ghc`: [fix `ia64` crashes on `foreign import wrapper`](https://gitlab.haskell.org/ghc/ghc/-/commit/e025c94f0dd63d2b944e502fa9c1d77094e49c89).

9 July: `ghc`: [fix `ia64` GC pointer chasing](https://gitlab.haskell.org/ghc/ghc/-/commit/d12690d5995de055d7e9b8ed04946bbb609b6e98).
On `ia64` pointers use higher bits of 64-bit pattern which broke garbage
collector assumptions.

8 July: `ghc`: [fix `alpha` build of `foreign import wrapper`](https://gitlab.haskell.org/ghc/ghc/-/commit/a8dc46dcbeeaf94a5321a1b8932725f7650d7abd).

## 2009

20 January: `mc`: [added `ebuild.syntax` highlighter](https://github.com/MidnightCommander/mc/commit/e0eb9ca1cd30cda67732096528e5573a14e5a1f4).

5 January: `hichi`: [started haskell implementation of an Intranet Chat protocol library](https://github.com/trofi/hichi).
I think I managed to implement a minimal bot. But never expanded it.

## 2007

24 November: `valgrind`: found a bug in [`rep lodsb` instruction emulation](https://bugs.kde.org/show_bug.cgi?id=152818).

4 June: `ichatd`: [started a linux implementation of Intranet Chat](https://github.com/trofi/ichatd).
Managed to use it successfully in LAN against Windows clients.
