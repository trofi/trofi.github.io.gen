---
title: "seekwatcher 0.15"
date: July 07, 2024
---

[`seekwatcher-0.15` is here](https://github.com/trofi/seekwatcher/releases/tag/v0.15)!

`seekwatcher` is a tool to visualize access to the block device.

It's been 2.5 years since [`seekwatcher-0.14` release](/posts/234-seekwatcher-0.14.html).
The only change is the switch from `mencoder` to `ffmpeg` tool. While at
it default codec is switched from `MPEG2` to `H264`.

As usual here is the program's result ran against `btrfs scrub` on my
device:

```
$ seekwatcher -t scrub.trace -p 'echo 3 > /proc/sys/vm/drop_caches; sync; btrfs scrub start -B /' -d /dev/nvme1n1p2
$ seekwatcher -t scrub.trace -o scrub.mpeg --movie
$ seekwatcher -t scrub.trace -o scrub.png
```

Outputs:

- [image](/posts.data/321-seekwatcher/scrub.png) (127K)
- [video](/posts.data/321-seekwatcher/scrub.mpeg) (926K)

`H264` makes video size comparable to the image report size.

Have fun!
