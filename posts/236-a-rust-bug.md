---
title: "A rust bug"
date: February 8, 2022
---

This is a post that documents a few tips to track down elusive problem
in a `rust` codebase of medium size: a few crates a few megabytes
of code each.

## The exhibit

It all started from seemingly minor problem: after an update of `rust`
from `1.56.1` to `1.57.0` test suite of `rav1e-0.4.1` project started
[failing in `nixpkgs`](https://github.com/NixOS/nixpkgs/pull/148358#issuecomment-985934315)

```
failures:
    src/api/config/mod.rs - api::config::Config::new_context (line 232)
    src/api/context.rs - api::context::Context<T>::new_frame (line 41)
    src/api/context.rs - api::context::Context<T>::receive_packet (line 197)
    src/api/context.rs - api::context::Context<T>::receive_packet (line 222)
    src/api/context.rs - api::context::Context<T>::send_frame (line 75)
```

Normally test suite failures are a better starting point than a bug on
real application. The inputs are usually trivial, they exercise small
part of the library, you could run a test under debugger and check the
place where crash happens and so on.
Here is how our failing test
[looks like](https://github.com/xiph/rav1e/blob/v0.5.1/src/api/context.rs#L278-L290):

````rust
/// ```
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// #     let mut enc = EncoderConfig::default();
/// #     // So it runs faster.
/// #     enc.width = 16;
/// #     enc.height = 16;
/// #     let cfg = Config::new().with_encoder_config(enc);
/// #     let mut ctx: Context<u8> = cfg.new_context()?;
/// #
/// #     let frames = vec![ctx.new_frame(); 4].into_iter();
/// #     encode_frames(&mut ctx, frames);
/// #
/// #     Ok(())
/// # }
/// ```
````

Looks simple. Testing against git checkout of `rav1e` shown the same failure:

```
$ cargo test --release
failures:
     ...
     src/api/context.rs - api::context::Context<T>::receive_packet (line 204)
     src/api/context.rs - api::context::Context<T>::receive_packet (line 229)
 test result: FAILED. 4 passed; 2 failed; 0 ignored; 0 measured; 0 filtered out; finished in 14.40s
```

Using this data Jörg filed [upstream report](https://github.com/xiph/rav1e/issues/2851)
at which point I thought the failure would be obvious to upstream developers.

## Into the rabbit hole

I'm usually wary of filing bugs where it's not very clear if it's a fault of
my environment or a fault of some underlying package way below the affected
one.
In this case it looked like a simple `rav1e` bug to my inexperienced eye.
Unfortunately upstream suspiction fell on `NixOS` almost immediately :)
Luckily others were able to reproduce the same failure with recent enough
compiler. The failure nature remained to be a complete mystery to others.
I gave it a try.

First, this test is a `doctest`: it's a part of source's comment that `doctest`
tool extracts, compiles, and runs. In theory if we do the same extraction
mechanically and write this code snippet to a text file it will fail the same.
It did not. Test kept failing only as a part of `doctest` run.
This proved to be a bit complicated to debug on release compiler: support for
collecting intermediate binaries from `doctest` is an unstable feature.
To avoid dealing with nightly compiler I resorted to patching the test with
`sleep()` to quickly hook into failure with a debugger in hopes of an obvious
bug:

````diff
--- a/src/api/context.rs
+++ b/src/api/context.rs
@@ -78,8 +78,10 @@ impl<T: Pixel> Context<T> {
   ///
   /// ```
   /// use rav1e::prelude::*;
+  /// use std::{thread, time};
   ///
   /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
+  /// thread::sleep(time::Duration::from_millis(1000 * 30));
   /// let cfg = Config::default();
   /// let mut ctx: Context<u8> = cfg.new_context().unwrap();
   /// let f1 = ctx.new_frame();
````

The result somewhat worked:

```
$ RUSTDOCFLAGS='-C debuginfo=2' cargo test --release --doc --verbose 'api::context::Context<T>::send_frame'
...
$ gdb -p $pid
(gdb) continue
Continuing.
[New Thread 0x7f4cf799e640 (LWP 503344)]
[New Thread 0x7f4cf779a640 (LWP 503345)]

Thread 2 "rust_out" received signal SIGSEGV, Segmentation fault.
[Switching to Thread 0x7f4cf799e640 (LWP 503344)]
0x00007f4cf799d9d0 in ?? ()
(gdb) bt
#0  0x00007f4cf799d9d0 in ?? ()
#1  0x00007f4cf799e5f8 in ?? ()
#2  0x00007f4cf799da80 in ?? ()
#3  0x000055b1d909d861 in rayon_core::registry::WorkerThread::set_current (thread=0x7f4cf799d600)
    at /home/slyfox/.cargo/registry/src/github.com-1ecc6299db9ec823/rayon-core-1.9.1/src/registry.rs:636
#4  rayon_core::registry::main_loop (registry=..., index=0, worker=...)
    at /home/slyfox/.cargo/registry/src/github.com-1ecc6299db9ec823/rayon-core-1.9.1/src/registry.rs:807
#5  rayon_core::registry::ThreadBuilder::run (self=...)
    at /home/slyfox/.cargo/registry/src/github.com-1ecc6299db9ec823/rayon-core-1.9.1/src/registry.rs:55
#6  0x000055b1d90a41dd in rayon_core::registry::{impl#2}::spawn::{closure#0} ()
    at /home/slyfox/.cargo/registry/src/github.com-1ecc6299db9ec823/rayon-core-1.9.1/src/registry.rs:100
#7  std::sys_common::backtrace::__rust_begin_short_backtrace<rayon_core::registry::{impl#2}::spawn::{closure#0}, ()> (f=...)
    at /build/rustc-1.57.0-src/library/std/src/sys_common/backtrace.rs:123
#8  0x000055b1d90975dc in std::thread::{impl#0}::spawn_unchecked::{closure#1}::{closure#0}<rayon_core::registry::{impl#2}::spawn::{closure#0}, ()> () at /build/rustc-1.57.0-src/library/std/src/thread/mod.rs:483
#9  core::panic::unwind_safe::{impl#23}::call_once<(), std::thread::{impl#0}::spawn_unchecked::{closure#1}::{closure#0}> (self=...,
    _args=<optimized out>) at /build/rustc-1.57.0-src/library/core/src/panic/unwind_safe.rs:271
#10 0x000055b1d90a11cf in std::panicking::try::do_call<core::panic::unwind_safe::AssertUnwindSafe<std::thread::{impl#0}::spawn_unchecked::{closure#1}::{closure#0}>, ()> (data=<optimized out>) at /build/rustc-1.57.0-src/library/std/src/panicking.rs:403
#11 std::panicking::try<(), core::panic::unwind_safe::AssertUnwindSafe<std::thread::{impl#0}::spawn_unchecked::{closure#1}::{closure#0}>>
    (f=<error reading variable: Cannot access memory at address 0x0>) at /build/rustc-1.57.0-src/library/std/src/panicking.rs:367
#12 0x000055b1d90a42d0 in std::panic::catch_unwind<core::panic::unwind_safe::AssertUnwindSafe<std::thread::{impl#0}::spawn_unchecked::{closure#1}::{closure#0}>, ()> (f=...) at /build/rustc-1.57.0-src/library/std/src/panic.rs:133
#13 0x000055b1d90960cb in std::thread::{impl#0}::spawn_unchecked::{closure#1}<rayon_core::registry::{impl#2}::spawn::{closure#0}, ()> ()
    at /build/rustc-1.57.0-src/library/std/src/thread/mod.rs:482
#14 core::ops::function::FnOnce::call_once<std::thread::{impl#0}::spawn_unchecked::{closure#1}, ()> ()
    at /build/rustc-1.57.0-src/library/core/src/ops/function.rs:227
#15 0x000055b1d91d65a5 in std::sys::unix::thread::Thread::new::thread_start ()
#16 0x00007f4cf83fdd40 in start_thread () from /nix/store/s9qbqh7gzacs7h68b2jfmn9l6q4jwfjz-glibc-2.33-59/lib/libpthread.so.0
#17 0x00007f4cf81e443f in clone () from /nix/store/s9qbqh7gzacs7h68b2jfmn9l6q4jwfjz-glibc-2.33-59/lib/libc.so.6
```

We got a detailed backtrace that gets into the depths of `rayon-core`
crate (it implements internals of parallel execution of tasks). I had
no idea what this trace showed me.
Having looked at the bits above `rav1e` upstream suggested filing a
bug against `rayon-core`.
By this time I realized there will be no easy way out and I'll have to
build something manageable to understand where the error really happens:
in `rav1e`, `rayon-core` or somewhere else.

**Quick quiz**: which component do you think will end up having a bug?

## Rust minimization HOWTO

So how does one shrink the example? My mechanical trick is to remove
dead code unrelated to our bug trigger.
`rust` has a useful feature of warning user of unused code.
Let's look at this toy example:

```rust
// cat a.rs
fn g(){}
pub fn pg(){}
fn main() {}
```

Building it:

```
$ rustc a.rs
warning: function is never used: `g`
 --> a.rs:1:4
  |
1 | fn g(){}
  |    ^
  |
  = note: `#[warn(dead_code)]` on by default

warning: 1 warning emitted
```

In this case `g()` is a clearly unused function: it's visibility is
limited to current module. Note that `pg()` is also unused for that
specific program. It's considered to be used because it's explicitly
exported for all external modules and crates.
To minimize a test we can safely assume that nothing should be exported
outside current crate except maybe `main()` function of the test itself.
Thus I came up with a hack: change all `pub`exports to `pub(crate)` exports
with a single `sed` line:

```
$ sed -e 's@pub @pub(crate) @g' -i *.rs
```

That's it! The rest compiler will do for us:

```
$ rustc a.rs
warning: function is never used: `g`
 --> a.rs:1:4
  |
1 | fn g(){}
  |    ^
  |
  = note: `#[warn(dead_code)]` on by default

warning: function is never used: `pg`
 --> a.rs:2:15
  |
2 | pub(crate) fn pg(){}
  |               ^^

warning: 2 warnings emitted
```

Yay! Now `pg()` is also reported as unused.
Now we just need to manually delete `pg()` definition from the source code
and make sure the hypothetical bug still triggers. Would be nice if
`cargo fix` removed this unused code automatically.
I have applied this `sed` hack to all of `rav1e` and almost immediately
got this reproducer:

````rust
// cat src/lib.rs
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;

/// # Examples
///
/// ```
/// use bug::do_bug;
///
/// # fn main() {
/// bug::do_bug()
/// # }
/// ```
pub fn do_bug() {
  (0..1).into_par_iter().for_each(|_| {});
  (0..1).into_par_iter().for_each(|_| {});
  (0..1).into_par_iter().for_each(|_| {});
}
````

Clearly a `rayon` bug, right? There is nothing `rav1e` specific in this code.
I filed the [bug report](https://github.com/rayon-rs/rayon/issues/911)
being quite confident I got the culprit. Alas once again suspiction fell
on my `NixOS` environment :)
I tried `ubuntu` and got the same `SIGSEGV` there. I felt that I'll need
to keep digging if I want this bug get solved. My example still relied on a
few other crates: `rayon-core`, standard library and something else.
I applied `pub(crate)` hack to `rayon-core` as well and got this beauty:

````rust
// cat src/lib.rs
thread_local! {
    static THREAD_LOCAL_GLOBAL: std::cell::Cell<usize> = std::cell::Cell::new(0);
}

#[inline(never)]
fn set_state_func(t: &std::cell::Cell<usize>) {
    t.set(42);
}

#[inline(never)]
fn thread_func() {
    THREAD_LOCAL_GLOBAL.with(set_state_func);
}

/// # Examples
///
/// ```
/// use bug::do_bug;
///
/// # fn main() {
/// bug::do_bug()
/// # }
/// ```
#[inline(never)]
pub fn do_bug() {
  // to ease catching the test with gdb
  //std::thread::sleep(std::time::Duration::from_secs(10));

  for _ in 0..128 {
    std::thread::spawn(thread_func).join().unwrap();
  }
}
````

I used `#[inline(never)]` to make sure we don't get affected by
optimizer decisions during reduction. I don't know if it has any
effect.
Here we spawn 128 no-op threads that set some thread-local global
variable. There is just nothing to break here! Note that `main()
still has to hide out in `doctest` comment to trigger `SIGSEGV`.

I filed the [bug against `rust`](https://github.com/rust-lang/rust/issues/92869).
In there Josh and Nikita quickly found the code generator discrepancy in
`llvm`
and fixed it with [`D117489` patch](https://reviews.llvm.org/D117489).

The bug was so elusive because `rav1e` `Cargo.toml` managed to
hit a few unique properties:

- build uses `debug = true / lto = "thin"` configuration in [profile.release]`
- `cargo test --doc --release` does not enable `-O` optimisations for tests
  but does enable `lto = "thin"` (a `cargo test` bug perhaps?). This matches
  neither debug nor release configuration for proper (non-`doctest`) tests.

## Parting words

Test case reduction is a simple and mechanical process for `rust` crates.

* Simple compiler bugs are a thing for `rust` as well as `c++`
* `gdb` renders meaningful stack frames for rust crashes.
* `pub` -> `pub(crate)` substitution is surprisingly effective for
  test minimization.
* `cargo test --doc --release` should be more consistent with `-O`
  optimization flags to make error less unique to `rustdoc`.
* `cargo fix` does not delete unused functions :)

Have fun!
