---
title: "Fixing wine-wayland on POLARIS"
date: November 28, 2022
---

After [switching to `wayland`](/posts/261-trying-out-wayland.html) I got
most GUI programs to use `wayland` backend render. The two main
exceptions are `pidgin` (used `gtk-2`) and `wine` (uses low level `x11`
primitives for many things). `pidgin` worked fine in `Xwayland`, but
`wine` did not always behave: sometimes input focus did not get passed
to the emulated application, sometimes wine could not use
`wayland`-native pixel screen dimensions and resorted back to
down-scaled dimensions.

There is an ongoing effort to get `wine` a new `wayland` backend by
Alexandros Frantzis and others. It's not yet merged to `wine` upstream
proper and is developed at <https://gitlab.collabora.com/alf/wine.git>
as a `wine` fork in `wayland`  branch. `nixpkgs` packages this fork as
`wine-wayland` package.

I tried `wine-wayland` for the first time this weekend. Unfortunately it
was not able to run `DirectX` or `OpenGL` games for windows. When I
started an application I heard expected sounds from the applications
(good) but got only black screen back (bad!).

I did not think I could fix it: `wine` was a huge project, I had no
background in modern graphics be it `linux` or `windows`. But I was
mildly curious about the following messages `wine` was dumping to
`stderr`:

```
$ wine foo.exe
00f0:err:waylanddrv:wayland_gl_drawable_update Failed to create GBM surface
00f0:err:waylanddrv:wayland_gl_drawable_update Failed to create EGL surface
011c:err:waylanddrv:wayland_gl_drawable_update Failed to create GBM surface
011c:err:waylanddrv:wayland_gl_drawable_update Failed to create EGL surface
```

I wanted to find out why `wine` was failing to create these things.

`GBM` and `EGL` acronyms sounded vaguely graphics-related. Creating the
rendering surface sounded like a simple procedure. Everything needs a
surface to draw anything. Maybe that error message is an actual bug?

`wine` has a great `WINEDEBUG=` mechanism to enable subsystem-specific
debug prints at runtime (`man wine` has detailed syntax description).
To get `wayland`-related debugging I used `WINEDEBUG=waylanddrv,opengl`:

```
$ WINEDEBUG=waylanddrv,opengl wine foo.exe
...
0124:trace:waylanddrv:wayland_gl_create_gbm_surface Using default format/modifier information
0124:trace:waylanddrv:wayland_gbm_create_surface 640x480 AR24 scanout=0 count_mods=2
0124:trace:waylanddrv:wayland_gbm_create_surface     mod: 0x00ffffffffffffff
0124:trace:waylanddrv:wayland_gbm_create_surface     mod: 0x0000000000000000
0124:err:waylanddrv:wayland_gl_drawable_update Failed to create GBM surface
0124:trace:waylanddrv:wayland_gl_drawable_update Failed to create EGL surface with SRGB colorspace, trying with default colorspace
0124:err:waylanddrv:wayland_gl_drawable_update Failed to create EGL surface
...
```

Not knowing much about these `GBM` things it felt like creating a `640x480`
surface with `AR24` format (8 bits for each of Red, Blue, Green, Alpha components)
should not be a problem and the call should succeed. Why does it fail here?

To ease exploration I started patching `mesa` and `wine-wayland` locally
with `fprintf(stderr, ...);` calls to see what gets passed around.

`nixpkgs` `mesa` clients use `/run/opengl-driver-32` paths to load
`opengl` `mesa` drivers. That means just rebuilding an application
against patched `mesa` locally in `nixpkgs` checkout is not enough to
get it used: applications will still load `/run/opengl-driver-32`
drivers. One day we will fix this non-hermeticity.

But until then we have to manually redirect used `mesa` from default
paths. There are many ways to do it. I used the following global
variables:

```
$ local_mesa=$(nix-build --no-link ~/nm -A pkgsi686Linux.mesa.drivers)
$ GBM_BACKENDS_PATH=/not-really-used \
  LIBGL_DRIVERS_PATH=$local_mesa/lib/dri \
  __EGL_VENDOR_LIBRARY_DIRS=$local_mesa/share/glvnd/egl_vendor.d \
      wine foo.exe
```

This allowed me to put patches to `~/nm` checkout of `nixpkgs` and
immediately observe their effect on `wine-wayland`.

I found that `wine` code at
[`winewayland.drv`](https://gitlab.collabora.com/alf/wine/-/blob/wayland/dlls/winewayland.drv/opengl.c#L373-L440)
calls `wayland_gbm_create_surface()` ->
[`wayland_gl_create_gbm_surface()`](https://gitlab.collabora.com/alf/wine/-/blob/wayland/dlls/winewayland.drv/opengl.c#L310-L371)
-> [`wayland_gbm_create_surface()`](https://gitlab.collabora.com/alf/wine/-/blob/wayland/dlls/winewayland.drv/gbm.c#L267-L299).
All the calls are shallow wrappers of one another. They just pass
through the request to create surface. I'll paste the latter in full
here:

```c
struct gbm_surface *wayland_gbm_create_surface(uint32_t drm_format, int width, int height,
                                               size_t count_modifiers, uint64_t *modifiers,
                                               BOOL format_is_scanoutable)
{
    uint32_t gbm_bo_flags = GBM_BO_USE_RENDERING;

    if (TRACE_ON(waylanddrv))
    {
        size_t i;

        TRACE("%dx%d %.4s scanout=%d count_mods=%zu\n",
              width, height, (const char *)&drm_format,
              format_is_scanoutable, count_modifiers);

        for (i = 0; i < count_modifiers; i++)
            TRACE("    mod: 0x%.16llx\n", (long long)modifiers[i]);
    }

    if (format_is_scanoutable) gbm_bo_flags |= GBM_BO_USE_SCANOUT;

    if (count_modifiers)
    {
#ifdef HAVE_GBM_SURFACE_CREATE_WITH_MODIFIERS2
        return gbm_surface_create_with_modifiers2(process_gbm_device, width, height,
                                                  drm_format, modifiers, count_modifiers, gbm_bo_flags);
#else
        return gbm_surface_create_with_modifiers(process_gbm_device, width, height,
                                                 drm_format, modifiers, count_modifiers);
#endif
    }

    return gbm_surface_create(process_gbm_device, width, height, drm_format, gbm_bo_flags);
}
```

Here `wayland_gbm_create_surface()` tries to call
`gbm_surface_create_with_modifiers2()` with 2 modifiers:

- unknown (tiled?): `0x00ffffffffffffff` (`DRM_FORMAT_MOD_INVALID`)
- linear: `0x0000000000000000` (`DRM_FORMAT_MOD_LINEAR`)

Note that if there would be no modifiers then `gbm_surface_create()`
would be called. From what I understand presence of extra modifiers is
optional (depends on a driver implementation).

From what I understand generic code should be able to work with all
modifiers. Picking wrong modifier should only affect performance (and
not correctness). An example modifier would be to enable tiled pixel
layout instead of default linear.

The fun thing is that `mesa` rejects
`gbm_surface_create_with_modifiers2()` call for me with `errno = ENOSYS`
at [`src/gbm/backends/dri/gbm_dri.c`](https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/gbm/backends/dri/gbm_dri.c#L1343-L1404):

```c
static struct gbm_surface *
gbm_dri_surface_create(struct gbm_device *gbm,
                       uint32_t width, uint32_t height,
                       uint32_t format, uint32_t flags,
                       const uint64_t *modifiers, const unsigned count)
{
   struct gbm_dri_device *dri = gbm_dri_device(gbm);
   struct gbm_dri_surface *surf;

   if (modifiers &&
       (!dri->image || dri->image->base.version < 14 ||
        !dri->image->createImageWithModifiers)) {
      errno = ENOSYS; // <- here
      return NULL;
   }
...
```

As I understand `mesa` assumes that `createImageWithModifiers` support
should be present in the backend driver. But in my case of
`GFX8 / POLARIS12` modifier support for image creation seems to be
disabled by `mesa` at
[`src/gallium/drivers/radeonsi/si_texture.c`](https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/gallium/drivers/radeonsi/si_texture.c#L2378-L2401).
Don't know why.

```c
void si_init_screen_texture_functions(struct si_screen *sscreen)
{
   sscreen->b.resource_from_handle = si_texture_from_handle;
   sscreen->b.resource_get_handle = si_texture_get_handle;
   sscreen->b.resource_get_param = si_resource_get_param;
   sscreen->b.resource_get_info = si_texture_get_info;
   sscreen->b.resource_from_memobj = si_resource_from_memobj;
   sscreen->b.memobj_create_from_handle = si_memobj_from_handle;
   sscreen->b.memobj_destroy = si_memobj_destroy;
   sscreen->b.check_resource_capability = si_check_resource_capability;
   sscreen->b.get_sparse_texture_virtual_page_size =
      si_get_sparse_texture_virtual_page_size;

   /* By not setting it the frontend will fall back to non-modifier create,
    * which works around some applications using modifiers that are not
    * allowed in combination with lack of error reporting in
    * gbm_dri_surface_create */
   if (sscreen->info.gfx_level >= GFX9 && sscreen->info.kernel_has_modifiers) {
      sscreen->b.resource_create_with_modifiers = si_texture_create_with_modifiers;
      sscreen->b.query_dmabuf_modifiers = si_query_dmabuf_modifiers;
      sscreen->b.is_dmabuf_modifier_supported = si_is_dmabuf_modifier_supported;
      sscreen->b.get_dmabuf_modifier_planes = si_get_dmabuf_modifier_planes;
   }
}
```

Here `GFX8` does not get a hint of `resource_create_with_modifiers` and
as a result [`src/gallium/frontends/dri/dri2.c`](https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/gallium/frontends/dri/dri2.c#L2196-L2264)
does not set `createImageWithModifiers`:

```c
static void
dri2_init_screen_extensions(struct dri_screen *screen,
                            struct pipe_screen *pscreen,
                            bool is_kms_screen)
{
   const __DRIextension **nExt;

   STATIC_ASSERT(sizeof(screen->screen_extensions) >=
                 sizeof(dri_screen_extensions_base));
   memcpy(&screen->screen_extensions, dri_screen_extensions_base,
          sizeof(dri_screen_extensions_base));
   screen->extensions = screen->screen_extensions;

   /* Point nExt at the end of the extension list */
   nExt = &screen->screen_extensions[ARRAY_SIZE(dri_screen_extensions_base)];

   screen->image_extension = dri2ImageExtensionTempl;
   if (pscreen->resource_create_with_modifiers) {
      screen->image_extension.createImageWithModifiers =
         dri2_create_image_with_modifiers;
      screen->image_extension.createImageWithModifiers2 =
         dri2_create_image_with_modifiers2;
   }
...
```

I'm not sure why `createImageWithModifiers` is skipped for me. Is it a
`mesa` bug? Is it expected because video card does not expose enough
information on tile format for `mesa` to reuse? I have no idea!

Anyway, from the above I was able to fix `wine-wayland` with one simple
trick by pretending there are no modifiers (assume `LINEAR`?):

```diff
--- a/dlls/winewayland.drv/gbm.c
+++ b/dlls/winewayland.drv/gbm.c
@@ -286,13 +286,20 @@ struct gbm_surface *wayland_gbm_create_surface(uint32_t drm_format, int width, i

     if (count_modifiers)
     {
+        errno = 0;
+        struct gbm_surface * s =
 #ifdef HAVE_GBM_SURFACE_CREATE_WITH_MODIFIERS2
-        return gbm_surface_create_with_modifiers2(process_gbm_device, width, height,
+               gbm_surface_create_with_modifiers2(process_gbm_device, width, height,
                                                   drm_format, modifiers, count_modifiers, gbm_bo_flags);
 #else
-        return gbm_surface_create_with_modifiers(process_gbm_device, width, height,
+               gbm_surface_create_with_modifiers(process_gbm_device, width, height,
                                                  drm_format, modifiers, count_modifiers);
 #endif
+        if (!s && errno == ENOSYS) {
+            TRACE("    => mesa can't create a surface with modifiers. Falling back to modifer-less.");
+        } else {
+            return s;
+        }
     }

     return gbm_surface_create(process_gbm_device, width, height, drm_format, gbm_bo_flags);
```

The idea is to just ignore `gbm_surface_create_with_modifiers2()`
result if it fails with `ENOSYS` and switch over to
`gbm_surface_create()` assuming it can handle the call without
modifier presence.

I'm not sure what such a fallback means for other video card types. Will
it break some tiled workloads? Or at this point surface creation is
already broken beyond repair and any try is better than nothing? I have
no idea!

With the hack I unexpectedly got my games back running on `wine-wayland`!

I sent above hack to Alexandros to get help where the real fix belongs.
Hopefully we can get `wine-wayland` to work on more GPUs than today.

## Parting words

Vanilla `wine` still has to run via `Xwayland` and it's not the best
experience for some picky applications. `wine-wayland` does a very
decent job at providing something that works (modulo cards like mine).
After I applied the fallback hack I was able to run all the games that
used to work on `wine-x11`.

`wine`'s `WINEDEBUG=` debugging facility is great at tracing both
application behavior and `wine`'s driver behavior. I had to add
surprisingly little extra debugging to what is already available in
`wine` in form of `TRACE(...);` calls.

`wine` error message was clear enough to understand why video
rendering did not produce anything.

`nix` ability to build patched `wine` against patched `mesa` without
destroying existing installation was critical for me to be able to do
side-by-side comparisons.

`mesa`'s `LIBGL_DRIVERS_PATH=` and `libglvnd`'s
`__EGL_VENDOR_LIBRARY_DIRS=` variables are ideal at fiddling with
patched `opengl` stack while keeping the main system running.

`wine`'s and `wayland`'s way of handling `linux` `DRM` subsystem is very
lightweight: they effectively pass available configuration around without
much of application-specific mangling. At least around surface creation
logic. That was very easy to get through.

Have fun!
