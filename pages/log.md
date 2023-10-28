---
title: log
---

Date ordered list of bugs and patches worth noting I did in various
projects:

23 July 2021: `linux`: `mm` [fix page_poison=1 data corruption](https://github.com/torvalds/linux/commit/69e5d322a2fb86173fde8bad26e8eb38cad1b1e9).
Without the change freeing a page always triggered assertions.

29 April 2021: `linux`: `mm` [fix data corruption caused by debug_pagealloc=1](https://github.com/torvalds/linux/commit/9df65f522536719682bccd24245ff94db956256c).
Without the fix zero-initialized pages were returned as `0xaa`-initialized.

29 April 2021: `linux`: `mm` [fix endless recursion tracking page_owner](https://github.com/torvalds/linux/commit/8e9b16c47680f6e7d6e5864a37f313f905a91cf5).

29 April 2021: `linux`: `ia64` [fix symbolizer crash](https://github.com/torvalds/linux/commit/99e729bd40fb3272fa4b0140839d5e957b58588a).

2 April 2021: `linux`: `ia64` [fix ptrace() stack pointer fetch](https://github.com/torvalds/linux/commit/7ad1e366167837daeb93d0bacb57dee820b0b898).

30 March 2021: `linux`: `hpsa` [fix ia64 boot failure for unaligmed SCSI commands](https://github.com/torvalds/linux/commit/02ec144292bc424a5800d45d4cb472c66e97c520).

12 March 2021: `linux`: `ia64` [fix ptrace(PTRACE_SYSCALL_INFO_EXIT) sign](https://github.com/torvalds/linux/commit/61bf318eac2c13356f7bd1c6a05421ef504ccc8a).

12 March 2021: `linux`: `ia64` [fix tracing of break-based syscalls](https://github.com/torvalds/linux/commit/0ceb1ace4a2778e34a5414e5349712ae4dc41d85).

10 March 2019: `linux`: `tty/vt` race condition crash [fix write/write race in ioctl(KDSKBSENT) handler](https://github.com/torvalds/linux/commit/46ca3f735f345c9d87383dd3a09fa5d43870770e)

31 Dec 2018: `linux`: `alpha` [fix page fault handling for r16-r18 registers](https://github.com/torvalds/linux/commit/491af60ffb848b59e82f7c9145833222e0bf27a5).
Without the change page fault handler corrupted wrong register.

1 May 2017: `linux`: `ia64` [fix module loading on gcc-5.4+](https://github.com/torvalds/linux/commit/a25fb8508c1b80dce742dbeaa4d75a1e9f2c5617).

7 April 2014: `linux`: [fix crash on thread pool remount](https://github.com/torvalds/linux/commit/800ee2247f483b6d05ed47ef3bbc90b56451746c).

13 September 2013: `linux`: [enable -Werror=implicit-int by default](https://github.com/torvalds/linux/commit/80970472179a45609c0b11b80619bc8c32b15f77).

24 April 2012: `linux`: `btrfs` [feature to change thread pool sizes](https://github.com/torvalds/linux/commit/0d2450abfa359ff94a2bee64a7daeba68c346c81).

16 April 2012: `linux`: `btrfs` [fix for remount data corruption](https://github.com/torvalds/linux/commit/8a3db1849e9e2563727ea2dc32737502e0096641).
Without the fix `btrfs` always triggered restore code if
`mount -oremount` fails for any reason.

25 August 2011: `linux`: `alpha` [fix for osf_setsysinfo()](https://github.com/torvalds/linux/commit/2df7a7d1cd07626dd235ca102830ebfc6c01a09e)
obscure interface.

20 May 2011: `linux`: `btrfs` [performance fix fix](https://github.com/torvalds/linux/commit/c4f675cd40d955d539180506c09515c90169b15b)
caused by spinning shrinker without a chance of progress.

11 April 2011: `linux`: `btrfs` [data corruption fix](https://github.com/torvalds/linux/commit/3387206f26e1b48703e810175b98611a4fd8e8ea)
caused by use of `memcpy()` on overlapping areas.

16 June 2009: `linux`: my first kernel [commit](https://github.com/torvalds/linux/commit/168f5ac668f63dfb64439766e3ef9e866b83719d).
It is a trivial `char *` to `const char *` substitution, but it's a
big milestone for me :)

20 January 2009: `mc`: added `ebuild.syntax` [andling to mc](https://github.com/MidnightCommander/mc/commit/e0eb9ca1cd30cda67732096528e5573a14e5a1f4).
