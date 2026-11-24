+++
title = "Broken chromium video acceleration"
date = "November 22, 2026"
tags = ["chromium", "bug", "wayland", "polaris", "amd"]
+++

## Tl;DR

`mesa-26.3.0` broke `chromium` video acceleration on `wayland`
on at least `AMD` `GFX8` chips (mine is `polaris12`). The easiest
workaround is to roll back `mesa` before
[`"radeonsi: Support DRM format modifiers on GFX6-8"`](https://gitlab.freedesktop.org/mesa/mesa/-/commit/12b7b819edb4ddd3580e7e5ffe384610ae726c90)
commit. `chromium` has a
[proposed fix for inclusion upstream](https://chromium-review.googlesource.com/c/chromium/src/+/8847181).

## More Words

### The Problem: Lost `GPU` Acceleration In `chromium`

Somehow I did not reboot my machine for a while. Once I had a chance to
do it I noticed that `chromium` browser took tens of seconds to start
(normally expected to start in a second). The scroll was sluggish.
Checking `chrome://gpu/` I found that video acceleration was disabled.
`stderr` complained about exit code `132` (`SIGILL`) in `GPU` process:

```
[1120/211159.034209:ERROR:third_party/crashpad/crashpad/util/process/process_memory_range.cc:75]
  read out of range
[2193:1:1120/211210.610049:ERROR:gpu/ipc/client/command_buffer_proxy_impl.cc:277]
  ContextResult::kTransientFailure: Failed to send GpuControl.CreateCommandBuffer.
[1980:1:1120/211210.610228:ERROR:gpu/ipc/client/command_buffer_proxy_impl.cc:277]
  ContextResult::kTransientFailure: Failed to send GpuControl.CreateCommandBuffer.
[1947:1:1120/211210.610346:ERROR:gpu/ipc/client/command_buffer_proxy_impl.cc:277]
  ContextResult::kTransientFailure: Failed to send GpuControl.CreateCommandBuffer.
[1821:1821:1120/211210.635637:ERROR:content/browser/gpu/gpu_process_host.cc:1042]
  GPU process exited unexpectedly: exit_code=132
[1120/211210.769569:ERROR:third_party/crashpad/crashpad/util/process/process_memory_range.cc:75]
  read out of range
[1821:1821:1120/211230.632702:ERROR:content/browser/gpu/gpu_process_host.cc:1042]
  GPU process exited unexpectedly: exit_code=132
[1120/211230.761576:ERROR:third_party/crashpad/crashpad/util/process/process_memory_range.cc:75]
  read out of range
[3388:1:1120/211242.124535:ERROR:gpu/ipc/client/command_buffer_proxy_impl.cc:277]
  ContextResult::kTransientFailure: Failed to send GpuControl.CreateCommandBuffer.
[3320:1:1120/211242.124621:ERROR:gpu/ipc/client/command_buffer_proxy_impl.cc:277]
  ContextResult::kTransientFailure: Failed to send GpuControl.CreateCommandBuffer.
[3277:1:1120/211242.124620:ERROR:gpu/ipc/client/command_buffer_proxy_impl.cc:476]
  GPU state invalid after WaitForGetOffsetInRange.
[1821:1821:1120/211242.131811:ERROR:content/browser/gpu/gpu_process_host.cc:1042]
  GPU process exited unexpectedly: exit_code=132
```

The slow startup is due to a long core dumping process. At least I was
able to get a backtrace:

```
$ coredumpctl debug chromium
...
Reading symbols from /nix/store/py8gk4jz9i4lnrmc25vjp7rcdbp9ma79-chromium-unwrapped-158.0.8127.36/libexec/chromium/chromium...
(No debugging symbols found in /nix/store/py8gk4jz9i4lnrmc25vjp7rcdbp9ma79-chromium-unwrapped-158.0.8127.36/libexec/chromium/chromium)
...
(gdb) bt
#0  0x0000648a4afb99ee in ui::NativePixmapEGLBinding::Create(scoped_refptr<gfx::NativePixmap>, viz::SharedImageFormat, std::__Cr::optional<int>, gfx::Size, gfx::ColorSpace const&, unsigned int, unsigned int) ()
#1  0x0000648a4b3a574d in ui::(anonymous namespace)::GLOzoneEGLWayland::ImportNativePixmap(scoped_refptr<gfx::NativePixmap>, viz::SharedImageFormat, std::__Cr::optional<int>, gfx::Size, gfx::ColorSpace const&, unsigned int, unsigned int) [clone .f7ea226dbd3986cbfff2a2e5fc4485d9] ()
#2  0x0000648a56988960 in gpu::OzoneImageGLTexturesHolder::CreateAndStoreTexture(gpu::SharedImageBacking*, scoped_refptr<gfx::NativePixmap>) ()
#3  0x0000648a56982686 in gpu::OzoneImageBacking::RetainGLTexturePerContextCache() ()
#4  0x0000648a569829be in gpu::OzoneImageBacking::ProduceSkiaGanesh(gpu::SharedImageManager*, gpu::MemoryTypeTracker*, scoped_refptr<gpu::SharedContextState>) ()
#5  0x0000648a56939fd1 in gpu::CompoundImageBacking::ProduceSkiaGanesh(gpu::SharedImageManager*, gpu::MemoryTypeTracker*, scoped_refptr<gpu::SharedContextState>) ()
#6  0x0000648a567deb66 in gpu::SharedImageBacking::ProduceSkia(gpu::SharedImageManager*, gpu::MemoryTypeTracker*, scoped_refptr<gpu::SharedContextState>) ()
#7  0x0000648a567dbf44 in gpu::SharedImageManager::ProduceSkia(gpu::Mailbox const&, gpu::MemoryTypeTracker*, scoped_refptr<gpu::SharedContextState>, gpu::SharedImageUsageSet) ()
#8  0x0000648a56933719 in gpu::SharedImageRepresentationFactory::ProduceSkia(gpu::Mailbox const&, scoped_refptr<gpu::SharedContextState>, gpu::SharedImageUsageSet) ()
#9  0x0000648a58057fe5 in viz::SkiaOutputSurfaceImplOnGpu::FinishPaintRenderPass(gpu::Mailbox const&, sk_sp<GrDeferredDisplayList>, sk_sp<GrDeferredDisplayList>, std::__Cr::unique_ptr<skgpu::graphite::Recording, std::__Cr::default_delete<skgpu::graphite::Recording> >, std::__Cr::vector<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1>, std::__Cr::allocator<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1> > >, base::OnceCallback<void ()>, base::OnceCallback<void (gfx::GpuFenceHandle)>, gfx::Rect const&, bool) ()
#10 0x0000648a5804a8da in base::internal::Invoker<base::internal::FunctorTraits<void (viz::SkiaOutputSurfaceImplOnGpu::*&&)(gpu::Mailbox const&, sk_sp<GrDeferredDisplayList>, sk_sp<GrDeferredDisplayList>, std::__Cr::unique_ptr<skgpu::graphite::Recording, std::__Cr::default_delete<skgpu::graphite::Recording> >, std::__Cr::vector<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1>, std::__Cr::allocator<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1> > >, base::OnceCallback<void ()>, base::OnceCallback<void (gfx::GpuFenceHandle)>, gfx::Rect const&, bool), viz::SkiaOutputSurfaceImplOnGpu*, gpu::Mailbox&&, sk_sp<GrDeferredDisplayList>&&, sk_sp<GrDeferredDisplayList>&&, std::__Cr::unique_ptr<skgpu::graphite::Recording, std::__Cr::default_delete<skgpu::graphite::Recording> >&&, std::__Cr::vector<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1>, std::__Cr::allocator<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1> > >&&, base::OnceCallback<void ()>&&, base::OnceCallback<void (gfx::GpuFenceHandle)>&&, gfx::Rect&&, bool&&>, base::internal::BindState<true, true, false, void (viz::SkiaOutputSurfaceImplOnGpu::*)(gpu::Mailbox const&, sk_sp<GrDeferredDisplayList>, sk_sp<GrDeferredDisplayList>, std::__Cr::unique_ptr<skgpu::graphite::Recording, std::__Cr::default_delete<skgpu::graphite::Recording> >, std::__Cr::vector<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1>, std::__Cr::allocator<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1> > >, base::OnceCallback<void ()>, base::OnceCallback<void (gfx::GpuFenceHandle)>, gfx::Rect const&, bool), base::internal::UnretainedWrapper<viz::SkiaOutputSurfaceImplOnGpu, base::unretained_traits::MayNotDangle, (partition_alloc::internal::RawPtrTraits)0>, gpu::Mailbox, sk_sp<GrDeferredDisplayList>, sk_sp<GrDeferredDisplayList>, std::__Cr::unique_ptr<skgpu::graphite::Recording, std::__Cr::default_delete<skgpu::graphite::Recording> >, std::__Cr::vector<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1>, std::__Cr::allocator<base::raw_ptr<viz::ImageContextImpl, (partition_alloc::internal::RawPtrTraits)1> > >, base::OnceCallback<void ()>, base::OnceCallback<void (gfx::GpuFenceHandle)>, gfx::Rect, bool>, void ()>::RunOnce(base::internal::BindStateBase*) [clone .cfi] ()
#11 0x0000648a5804b622 in base::internal::Invoker<base::internal::FunctorTraits<viz::SkiaOutputSurfaceImpl::FlushGpuTasksWithImpl(viz::SkiaOutputSurfaceImpl::SyncMode, viz::SkiaOutputSurfaceImplOnGpu*, gpu::SyncToken const&)::$_0&&, std::__Cr::vector<base::OnceCallback<void ()>, std::__Cr::allocator<base::OnceCallback<void ()> > >&&, viz::SkiaOutputSurfaceImpl::SyncMode&&, base::WaitableEvent*&&, viz::SkiaOutputSurfaceImplOnGpu*&&, bool&&, bool&&, base::TimeTicks&&>, base::internal::BindState<false, false, false, viz::SkiaOutputSurfaceImpl::FlushGpuTasksWithImpl(viz::SkiaOutputSurfaceImpl::SyncMode, viz::SkiaOutputSurfaceImplOnGpu*, gpu::SyncToken const&)::$_0, std::__Cr::vector<base::OnceCallback<void ()>, std::__Cr::allocator<base::OnceCallback<void ()> > >, viz::SkiaOutputSurfaceImpl::SyncMode, base::internal::UnretainedWrapper<base::WaitableEvent, base::unretained_traits::MayNotDangle, (partition_alloc::internal::RawPtrTraits)0>, base::internal::UnretainedWrapper<viz::SkiaOutputSurfaceImplOnGpu, base::unretained_traits::MayNotDangle, (partition_alloc::internal::RawPtrTraits)0>, bool, bool, base::TimeTicks>, void ()>::RunOnce(base::internal::BindStateBase*) [clone .2cba30843f198ee40e0524c326a2475d] [clone .cfi] ()
#12 0x0000648a4e001a71 in gpu::Scheduler::RunNextTask() ()
#13 0x0000648a53348041 in base::TaskAnnotator::RunTaskImpl(base::PendingTask&) ()
#14 0x0000648a5336d3fb in base::sequence_manager::internal::ThreadControllerWithMessagePumpImpl::DoWork() ()
#15 0x0000648a532e669d in base::MessagePumpDefault::Run(base::MessagePump::Delegate*) ()
#16 0x0000648a5336e192 in base::sequence_manager::internal::ThreadControllerWithMessagePumpImpl::Run(bool, base::TimeDelta) ()
#17 0x0000648a53320351 in base::RunLoop::Run(base::Location const&) ()
#18 0x0000648a57fcd522 in content::GpuMain(content::MainFunctionParams) [clone .cfi] ()
#19 0x0000648a5329eeb5 in content::RunOtherNamedProcessTypeMain(std::__Cr::basic_string<char, std::__Cr::char_traits<char>, std::__Cr::allocator<char> > const&, content::MainFunctionParams, content::ContentMainDelegate*) ()
#20 0x0000648a532a0ef3 in content::ContentMainRunnerImpl::Run() ()
#21 0x0000648a5329d81c in content::ContentMain(content::ContentMainParams) ()
#22 0x0000648a4a6aa9dd in ChromeMain ()
#23 0x000076fc57a2b391 in __libc_start_call_main () from /nix/store/r3hvhzl5fxhrgn0rfy5g7rda17702pj4-glibc-2.44-57/lib/libc.so.6
#24 0x000076fc57a2b4a8 in __libc_start_main_impl () from /nix/store/r3hvhzl5fxhrgn0rfy5g7rda17702pj4-glibc-2.44-57/lib/libc.so.6
#25 0x0000648a4a1d4025 in _start ()
```

Looks like a failure at texture creation time.

### Past Failures

It's not the first time I had problems of a very similar sort. Last
instance was on `chromium-140.0.7339.185` where it
[failed to create GPU-compatible surfaces](https://issues.chromium.org/issues/446395287):

```
[2534001:2534001:0920/215028.731999:ERROR:ui/ozone/platform/wayland/gpu/gbm_pixmap_wayland.cc:82] :
  Cannot create bo with format= RGBA_8888 and usage=SCANOUT
[2534001:2534001:0920/215028.732103:ERROR:ui/ozone/platform/wayland/gpu/gbm_pixmap_wayland.cc:82] :
  Cannot create bo with format= RGBA_8888 and usage=GPU_READ
```

Even by that time I already had some experience hacking around the same
issue in [`wine-wayland`](/posts/268-fixing-wine-wayland-on-polaris.html).

### What Triggered the Regression?

I was a bit confused as most other `OpenGL` programs worked just fine.
Why the `chromium` is special here? To restore my browser I attempted to
pinpoint the component. I keep `90` days of past OS system builds and
build the systems every night. I had `50` generations to bisect through.
After `7` reboots I found that `November 10` version was fine while
`November 11` version was not. From the system generations I extracted
`nixpkgs` commits my system was built from (`nixos-version` has it as is
as last 6 characters):

```
/nix/var/nix/profiles/system-2896-link/nixos-version: 26.11.20261111.7990b0f
/nix/var/nix/profiles/system-2895-link/nixos-version: 26.11.20261110.dd65537
```

The bisection to an individual commit was trivial:

```
$ git bisect start 7990b0f dd65537
821 revisions, 10 steps

$ sudo nixos-rebuild switch --impure --override-input nixpkgs .
<reboot>
```

It took me `10` steps (about `25` minutes for all of them) to arrive at
[`mesa: 26.2.6 -> 26.3.0`](https://github.com/NixOS/nixpkgs/commit/1eebd1524576432e125e525a83eec1272540711c)
update. Luckily `mesa` is not a build dependency of most packages and I
had almost nothing to rebuild on any of these steps.
Before just reverting `mesa` locally I was curious what in `mesa` caused
`chromium` to break. I bisected `mesa` as well. I checked out `mesa`
locally to start a bisect:

```
$ git bisect start mesa-26.3.0 mesa-26.2.6
13 steps
```

And I did a 2-liner change in `nixpkgs` to use `git` tree of `mesa`:

```diff
--- a/pkgs/development/libraries/mesa/common.nix
+++ b/pkgs/development/libraries/mesa/common.nix
@@ -7,13 +7,14 @@ rec {
   pname = "mesa";
   version = "26.3.1";

-  src = fetchFromGitLab {
+  src_ = fetchFromGitLab {
     domain = "gitlab.freedesktop.org";
     owner = "mesa";
     repo = "mesa";
     rev = "mesa-${version}";
     hash = "sha256-wOftyyAhQT/gtCZrmVPnqkSPvUKJlSExTe0++5MZj44=";
   };
+  src = builtins.fetchGit /mnt/archive/big/git/mesa;

   meta = {
     description = "Open source 3D graphics library";
```

I used the same command to rebuild and test the system:

```
$ sudo nixos-rebuild switch --impure --override-input nixpkgs .
<reboot>
```

Each iteration took me about 6 minutes as `mesa` needs about `6000`
source files to build in `nixpkgs`. I removed drivers I don't use as:

```diff
--- a/pkgs/development/libraries/mesa/default.nix
+++ b/pkgs/development/libraries/mesa/default.nix
@@ -55,43 +55,12 @@
   # We enable as many drivers as possible here, to build cross tools
   # and support emulation use cases (emulated x86_64 on aarch64, etc)
   galliumDrivers ? [
-    "asahi" # Apple AGX
-    "crocus" # Intel legacy
-    "d3d12" # WSL emulated GPU (aka Dozen)
-    "ethosu" # ARM Ethos NPU
-    "etnaviv" # Vivante GPU designs (mostly NXP/Marvell SoCs)
-    "freedreno" # Qualcomm Adreno (all Qualcomm SoCs)
-    "i915" # Intel extra legacy
-    "iris" # new Intel (Broadwell+)
-    "lima" # ARM Mali 4xx
-    "llvmpipe" # software renderer
-    "nouveau" # Nvidia
-    "panfrost" # ARM Mali Midgard and up (T/G series)
     "r300" # very old AMD
     "r600" # less old AMD
     "radeonsi" # new AMD (GCN+)
-    "rocket" # Rockchip NPU
-    "softpipe" # older software renderer
-    "svga" # VMWare virtualized GPU
-    "tegra" # Nvidia Tegra SoCs
-    "v3d" # Broadcom VC5 (Raspberry Pi 4)
-    "vc4" # Broadcom VC4 (Raspberry Pi 0-3)
-    "virgl" # QEMU virtualized GPU (aka VirGL)
-    "zink" # generic OpenGL over Vulkan, experimental
   ],
   vulkanDrivers ? [
     "amd" # AMD (aka RADV)
-    "asahi" # Apple AGX
-    "broadcom" # Broadcom VC5 (Raspberry Pi 4, aka V3D)
-    "freedreno" # Qualcomm Adreno (all Qualcomm SoCs)
-    "gfxstream" # Android virtualized GPU
-    "imagination" # PowerVR Rogue (currently N/A)
-    "intel_hasvk" # Intel Haswell/Broadwell, "legacy" Vulkan driver (https://www.phoronix.com/news/Intel-HasVK-Drop-Dead-Code)
-    "intel" # new Intel (aka ANV)
-    "microsoft-experimental" # WSL virtualized GPU (aka DZN/Dozen)
-    "nouveau" # Nouveau (aka NVK)
-    "panfrost" # ARM Mali Midgard and up (T/G series)
-    "swrast" # software renderer (aka Lavapipe)
   ]
   ++
     lib.optionals
```

This shrunk `mesa` builds down from `6000` to `2200` files and sped up
build from 6 minutes to 2 minutes. After 12 steps I arrived at the
suspicious
[`"radeonsi: Support DRM format modifiers on GFX6-8"`](https://gitlab.freedesktop.org/mesa/mesa/-/commit/12b7b819edb4ddd3580e7e5ffe384610ae726c90)
commit:

```
radeonsi: Support DRM format modifiers on GFX6-8

Now that ac_surface supports GFX6-8 modifiers,
enable them in RadeonSI too.

Part-of: <https://gitlab.freedesktop.org/mesa/mesa/-/merge_requests/41557>
```

At this point I realized the actual triggering change was in my
compositor (`niri`) and not in `chromium` behaviour. Before the patch
`niri` reported only one supported (`INVALID`) modifier per pixel format:

```
$ wayland-info | grep -A20 zwp_linux_dmabuf_v1
interface: 'zwp_linux_dmabuf_v1',                        version:  5, name: 42
        main device: 0xE280 (/dev/dri/card1 or /dev/dri/renderD128)
        tranche
                target device: 0xE280 (/dev/dri/card1 or /dev/dri/renderD128)
                flags: none
                formats (fourcc) and modifiers (names):
                0x48344241 = 'AB4H'; 0x00ffffffffffffff = INVALID
                0x48344258 = 'XB4H'; 0x00ffffffffffffff = INVALID
                0x30335241 = 'AR30'; 0x00ffffffffffffff = INVALID
                0x30335258 = 'XR30'; 0x00ffffffffffffff = INVALID
                0x30334241 = 'AB30'; 0x00ffffffffffffff = INVALID
                0x30334258 = 'XB30'; 0x00ffffffffffffff = INVALID
                0x34325241 = 'AR24'; 0x00ffffffffffffff = INVALID
                0x34324142 = 'BA24'; 0x00ffffffffffffff = INVALID
                0x34324241 = 'AB24'; 0x00ffffffffffffff = INVALID
```

After the patch a few more modes appeared per pixel format:

```
$ wayland-info | grep -A20 zwp_linux_dmabuf_v1
interface: 'zwp_linux_dmabuf_v1',                        version:  5, name: 42
        main device: 0xE280 (/dev/dri/card1 or /dev/dri/renderD128)
        tranche
                target device: 0xE280 (/dev/dri/card1 or /dev/dri/renderD128)
                flags: none
                formats (fourcc) and modifiers (names):
                0x48344241 = 'AB4H'; 0x02000001c90a6400 = AMD_UNKNOWN_MODIFIER
                0x48344241 = 'AB4H'; 0x02000001c90a4400 = AMD_UNKNOWN_MODIFIER
                0x48344241 = 'AB4H'; 0x02000001c90a0400 = AMD_UNKNOWN_MODIFIER
                0x48344241 = 'AB4H'; 0x0200000000004200 = AMD_UNKNOWN_MODIFIER
                0x48344241 = 'AB4H'; 0x0200000000000200 = AMD_UNKNOWN_MODIFIER
                0x48344241 = 'AB4H'; 0x0000000000000000 = LINEAR
                0x48344241 = 'AB4H'; 0x00ffffffffffffff = INVALID
                0x48344258 = 'XB4H'; 0x02000001c90a6400 = AMD_UNKNOWN_MODIFIER
                0x48344258 = 'XB4H'; 0x02000001c90a4400 = AMD_UNKNOWN_MODIFIER
                0x48344258 = 'XB4H'; 0x02000001c90a0400 = AMD_UNKNOWN_MODIFIER
                0x48344258 = 'XB4H'; 0x0200000000004200 = AMD_UNKNOWN_MODIFIER
                0x48344258 = 'XB4H'; 0x0200000000000200 = AMD_UNKNOWN_MODIFIER
                0x48344258 = 'XB4H'; 0x0000000000000000 = LINEAR
                0x48344258 = 'XB4H'; 0x00ffffffffffffff = INVALID
                0x30335241 = 'AR30'; 0x02000001d0ca6400 = AMD_UNKNOWN_MODIFIER
```

I was not sure where the bug is: in `niri`, in `mesa`, in `chromium` or
maybe in `linux` kernel `GPU` in how it handles other modes?
Looking around `mesa` bug tracker I found
[`"[radeonsi/GFX6] Regression 26.2.6 → 26.3.1: Chromium-based browser GPU process dies with SIGILL on Oland [1002:6613]; downgrading Mesa fixes it"`](https://gitlab.freedesktop.org/mesa/mesa/-/work_items/16942)
bug which looks identical to mine.
A mystery! I reported my setup in [the comment](https://gitlab.freedesktop.org/mesa/mesa/-/work_items/16942#note_3799602).
And day later someone reported that `chromium` already has the fix
[proposed for inclusion](https://chromium-review.googlesource.com/c/chromium/src/+/8847181):

```diff
[PATCH] [ozone] Don't range-check DRM modifier halves as EGLint

NativePixmapEGLBinding::InitializeFromNativePixmap() pushed the low
and high 32 bits of the format modifier through
base::checked_cast<EGLint>(), which CHECK-fails when a half has bit 31
set. AMD GFX8 (Polaris) modifiers from recent Mesa, such as
0x02000001D0CA0400, hit this and crash the GPU process.

EGL_EXT_image_dma_buf_import_modifiers expects the raw bit pattern of
each half, so reinterpret the uint32_t halves as EGLint with
base::bit_cast instead. Offsets keep the checked conversion.

Fixed: 588420152
Change-Id: Icaf1cdf50969175f685f54c87adcada7248dbd93
---

diff --git a/ui/ozone/common/native_pixmap_egl_binding.cc b/ui/ozone/common/native_pixmap_egl_binding.cc
index d0807f8d..8b9a148 100644
--- a/ui/ozone/common/native_pixmap_egl_binding.cc
+++ b/ui/ozone/common/native_pixmap_egl_binding.cc
@@ -6,6 +6,7 @@

 #include <array>

+#include "base/bit_cast.h"
 #include "base/logging.h"
 #include "base/memory/scoped_refptr.h"
 #include "base/notreached.h"
@@ -236,10 +237,14 @@
               base::NotFatalUntil::M163);
         CHECK(attrs_plane < std::size(kPlaneHiModifierAttrs),
               base::NotFatalUntil::M163);
+        // EGL takes the raw bits of each 32-bit half of the modifier, which
+        // may have the sign bit set (e.g. AMD modifiers).
         attrs.push_back(kPlaneLoModifierAttrs[attrs_plane]);
-        PushEGLintOrDie(modifier & 0xffffffff, attrs);
+        attrs.push_back(
+            base::bit_cast<EGLint>(static_cast<uint32_t>(modifier)));
         attrs.push_back(kPlaneHiModifierAttrs[attrs_plane]);
-        PushEGLintOrDie(static_cast<uint32_t>(modifier >> 32), attrs);
+        attrs.push_back(
+            base::bit_cast<EGLint>(static_cast<uint32_t>(modifier >> 32)));
       }
     }
     attrs.push_back(EGL_NONE);
```

Gah! A simple `C++` coding bug in handling sign extension!

### More Targeted Workaround

To restore video acceleration without full rollback of `mesa` I settled
on a single patch revert in my local system via this overlay definition:

```nix
# cat /etc/nixos/overlay.nix
final: prev: {
  mesa = prev.mesa.overrideAttrs(oa: {
    patches = oa.patches ++ [
      # fix chrome hwaccel
      (prev.fetchpatch {
        name = "revert-AMD_UNKNOWN_MODIFIER.patch";
        url = "https://gitlab.freedesktop.org/mesa/mesa/-/commit/12b7b819edb4ddd3580e7e5ffe384610ae726c90.patch";
        revert = true;
        hash = "sha256-VJAaqgbjlfhjg++2DjG1ZXrE6o7HnZrToJBCPEBAPks=";
      })
    ];
  });
}
```

Another option would be to apply pending `chromium` patch, but `nixpkgs`
does [not make it easy](https://github.com/NixOS/nixpkgs/issues/276884)
and `chromium` builds are quite long.

### Where Do These Modes come From?

`0x0000000000000000 = LINEAR` and `0x00ffffffffffffff = INVALID`
are just two constants. But `AMD_UNKNOWN_MODIFIER` has many different
values:

```
0x02000001c90a6400 = AMD_UNKNOWN_MODIFIER
0x02000001c90a4400 = AMD_UNKNOWN_MODIFIER
0x02000001c90a0400 = AMD_UNKNOWN_MODIFIER
0x0200000000004200 = AMD_UNKNOWN_MODIFIER
0x0200000000000200 = AMD_UNKNOWN_MODIFIER
```

Where do they come from and what do they mean? My understanding is that
it comes from `linux` `DRM` subsystem:

```
$ drm_info
...

└───Planes
    ├───Plane 0
...
    │       ├───"IN_FORMATS" (immutable): blob = 45
    │       │   ├───AMD(TILE_VERSION = unknown, TILE = unknown) (0x02000001d08a0400)
    │       │   │   └───RGB565 (0x36314752)
    │       │   ├───AMD(TILE_VERSION = unknown, TILE = unknown) (0x02000001d0ca0400)
    │       │   │   ├───XRGB8888 (0x34325258)
    │       │   │   ├───ARGB8888 (0x34325241)
    │       │   │   ├───RGBA8888 (0x34324152)
    │       │   │   ├───XRGB2101010 (0x30335258)
    │       │   │   ├───XBGR2101010 (0x30334258)
    │       │   │   ├───ARGB2101010 (0x30335241)
    │       │   │   ├───ABGR2101010 (0x30334241)
    │       │   │   ├───XBGR8888 (0x34324258)
    │       │   │   └───ABGR8888 (0x34324241)
    │       │   ├───AMD(TILE_VERSION = unknown, TILE = unknown) (0x02000001c90a0400)
    │       │   │   ├───XRGB16161616 (0x38345258)
    │       │   │   ├───XBGR16161616 (0x38344258)
    │       │   │   ├───ARGB16161616 (0x38345241)
    │       │   │   ├───ABGR16161616 (0x38344241)
    │       │   │   ├───XRGB16161616F (0x48345258)
    │       │   │   ├───ARGB16161616F (0x48345241)
    │       │   │   ├───XBGR16161616F (0x48344258)
    │       │   │   └───ABGR16161616F (0x48344241)
    │       │   ├───AMD(TILE_VERSION = unknown, TILE = unknown) (0x0200000000000200)
    │       │   │   ├───XRGB8888 (0x34325258)
    │       │   │   ├───ARGB8888 (0x34325241)
    │       │   │   ├───RGBA8888 (0x34324152)
    │       │   │   ├───XRGB2101010 (0x30335258)
    │       │   │   ├───XBGR2101010 (0x30334258)
    │       │   │   ├───ARGB2101010 (0x30335241)
    │       │   │   ├───ABGR2101010 (0x30334241)
    │       │   │   ├───XRGB16161616 (0x38345258)
    │       │   │   ├───XBGR16161616 (0x38344258)
    │       │   │   ├───ARGB16161616 (0x38345241)
    │       │   │   ├───ABGR16161616 (0x38344241)
    │       │   │   ├───XBGR8888 (0x34324258)
    │       │   │   ├───ABGR8888 (0x34324241)
    │       │   │   ├───RGB565 (0x36314752)
    │       │   │   ├───XRGB16161616F (0x48345258)
    │       │   │   ├───ARGB16161616F (0x48345241)
    │       │   │   ├───XBGR16161616F (0x48344258)
    │       │   │   └───ABGR16161616F (0x48344241)
    │       │   └───DRM_FORMAT_MOD_LINEAR (0x0000000000000000)
    │       │       ├───XRGB8888 (0x34325258)
...
```

I was wondering why `AMD(TILE_VERSION = unknown, TILE = unknown) (0x02000001d08a0400)`
does not show any details about `TILE_VERSION` / `TILE` and found that
`libdrm` did not yet merge support for it at
[`"amdgpu: Print new DRM format modifiers for AMD GFX6-8"`](https://gitlab.freedesktop.org/mesa/libdrm/-/merge_requests/481).
The header change has a good description of bits:

```
 * DRM format modifier fields on AMD GPUs:
 *     7:0 TILE_VERSION             Values are AMD_FMT_MOD_TILE_VER_*
 *    12:8 TILE                     Values are AMD_FMT_MOD_TILE_<version>_*
 *      13 DCC                      Delta Color Compression, supported on GFX8 and newer
 *   55:14 (chip specific)          See below for details, depends on GFX block version
 *   63:56 Vendor                   Value is DRM_FORMAT_MOD_VENDOR_AMD
...
 * Chip specific fields on Gfx6-8:
 *   16:14 MICROTILE                Micro tile format
 *   21:17 PIPE_CONFIG              Number of pipes and how pipes are interleaved
 *   24:22 TILE_SPLIT               Tile split size
 *   26:25 BANK_WIDTH               Number of tiles in the X direction in the same bank
 *   28:27 BANK_HEIGHT              Number of tiles in the Y direction in the same bank
 *   30:29 MACRO_TILE_ASPECT        Macro tile aspect ratio
 *   32:31 NUM_BANKS                Number of banks
 *   55:33 -                        Reserved for future use, must be zero
```

I also found that `linux` kernel got the support for reporting these
tile modes in [2026](https://lists.freedesktop.org/archives/amd-gfx/2026-May/144535.html):

```
$ git tag --contains=3104069690c499ffa1fc3de998c6d0acf24ee350
v7.3
v7.3-rc1
...
```

There are a few relevant commits in `linux`:

- [`drm/amd/display: Support DRM format modifiers on GFX6-8`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=3104069690c499ffa1fc3de998c6d0acf24ee350)
- [`drm/amdgpu: Convert tiling flags to modifiers on GFX6-8`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=24c097b1123a29d6cee10c2c15cd4f52f870e83f)
- [`drm/fourcc: Add modifiers for AMD GFX6-8`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=6a03efb8613a6ffc23da42af41b3e51e975ea406)

## Parting Words

Timur Kristóf is busy updating `Linux` ecosystem to expose to compositors
various tiling modes available on older `AMD` `GPU` cards like my `radeonsi`
(`GFX8`). [This commit message](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=6a03efb8613a6ffc23da42af41b3e51e975ea406)
is a great overview of the motivation behind the changes and overview of
some limitations. That's very exciting!
As these changes slowly trickle into released versions of software it
exposes bugs on the software. In this case it was a
[`chromium` bug](https://chromium-review.googlesource.com/c/chromium/src/+/8847181).

Reverting [`"radeonsi: Support DRM format modifiers on GFX6-8"`](https://gitlab.freedesktop.org/mesa/mesa/-/commit/12b7b819edb4ddd3580e7e5ffe384610ae726c90)
in `mesa` allowed me to restore `GPU` acceleration in `chromium`. I
suspect using the old kernel would also work around the failure.
I did not test the `chromium` fix yet, but it looks like it should work
as well.

Have fun!
