---
title: "Unexpected runtime dependencies in nixpkgs"
date: September 14, 2023
---

## Intro

`nix` uses a bit unusual way to detect runtime dependencies: it scans
the build result for it's hash inputs used during build and does not
require manual specification of the dependencies.

For example if we build a trivial package that just prints a string into
the file it automatically pulls in that strings into runtime closure:

```
$ nix build --impure --expr 'with import <nixpkgs> {}; runCommand "foo" {} \
  "echo ${re2c}/bin/re2c > $out"'
```

This command builds a package that consists of a single `$out` file
(symlinked to `./result`) which contains absolute path to `re2c` binary.
It's not a complicated package:

```
$ cat result
/nix/store/pj9cdgj07iz3cj88rywapx2lfxfmdqd3-re2c-3.1/bin/re2c
```

And yet if we look at full closure of it's inferred dependencies it
already has `re2c` and all it's runtime dependencies:

```
$ nix path-info -r ./result
/nix/store/gnzwqa9df994g01yw5x75qnbl1rhp9ds-libunistring-1.1
/nix/store/h3aw16j1c54jv8s39yvdhpfcx3538jwi-libidn2-2.3.4
/nix/store/kv0v4h5i911gj39m7n9q10k8r8gbn3sa-xgcc-12.3.0-libgcc
/nix/store/905gkx2q1pswixwmi1qfhfl6mik3f22l-glibc-2.37-8
/nix/store/s2pgr9iqj60mfnmabixnqacxl4bzb408-gcc-12.3.0-libgcc
/nix/store/gi26p79iq8jrw51irq5x82c2cqlgicxi-gcc-12.3.0-lib
/nix/store/pj9cdgj07iz3cj88rywapx2lfxfmdqd3-re2c-3.1
/nix/store/amiqn0hvnmrfcz2s8b47fb770v8hy9ny-foo
```

Such automatic scanning method method has both false positives and false
negatives.

Say, if we are to compress the file reference might disappear:

```
$ nix build --impure --expr 'with import <nixpkgs> {}; runCommand "foo" {} \
  "echo ${re2c}/bin/re2c | bzip2 > $out"'

$ nix path-info -r ./result
/nix/store/lwx722djnam7yjy439b9k6czb55h707q-foo
```

Missing reference detection is a bug here (false negative). False
negatives can be worked around by explicitly adding plain text
references into the file just like we did in the original example.

But I would like to talk about false positives today.

## The problem

Let's jump start from the motivating example: right now `nix` package
has header only `nlohmann/json` dependency in it's runtime closure:

```
$ nix path-info -r nixpkgs#nix | fgrep nlohmann_json
/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2
```

There is nothing in `nlohmann/json` useful for `nix`'s runtime:

```
$ find /nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2 -type f | unnix
/<<NIX>>/nlohmann_json-3.11.2/include/nlohmann/detail/conversions/from_json.hpp
/<<NIX>>/nlohmann_json-3.11.2/include/nlohmann/detail/conversions/to_chars.hpp
/<<NIX>>/nlohmann_json-3.11.2/include/nlohmann/detail/conversions/to_json.hpp
...
/<<NIX>>/nlohmann_json-3.11.2/include/nlohmann/ordered_map.hpp
/<<NIX>>/nlohmann_json-3.11.2/share/cmake/nlohmann_json/nlohmann_jsonTargets.cmake
/<<NIX>>/nlohmann_json-3.11.2/share/cmake/nlohmann_json/nlohmann_jsonConfig.cmake
/<<NIX>>/nlohmann_json-3.11.2/share/cmake/nlohmann_json/nlohmann_jsonConfigVersion.cmake
/<<NIX>>/nlohmann_json-3.11.2/share/pkgconfig/nlohmann_json.pc
```

These are a few headers and `cmake` and `pkg-config` plumbing.

So why does `nix` retain those then?

## Debugging the details

To figure out where the references come from we can grep the package for
a raw string and see how it gets there:

```
$ LANG=C grep -R $(nix-build --no-link '<nixpkgs>' -A nlohmann_json) $(nix-build --no-link '<nixpkgs>' -A nix.out)
grep: /nix/store/vxx4c6gc2zgfw870b40f06dmli6ljp34-nix-2.17.0/bin/nix: binary file matches
grep: /nix/store/vxx4c6gc2zgfw870b40f06dmli6ljp34-nix-2.17.0/bin/nix-build: binary file matches
grep: /nix/store/vxx4c6gc2zgfw870b40f06dmli6ljp34-nix-2.17.0/bin/nix-channel: binary file matches
grep: /nix/store/vxx4c6gc2zgfw870b40f06dmli6ljp34-nix-2.17.0/bin/nix-collect-garbage: binary file matches
grep: /nix/store/vxx4c6gc2zgfw870b40f06dmli6ljp34-nix-2.17.0/bin/nix-copy-closure: binary file matches
...
```

Here we see that even `nix` binary itself retains `nlohmann/json`
reference. With `strings` tool from `GNU binutils` we can check how the
reference looks like:

```
$ nix shell nixpkgs#binutils-unwrapped
$$strings $(nix-build --no-link '<nixpkgs>' -A nix.out)/bin/nix | grep $(nix-build --no-link '<nixpkgs>' -A nlohmann_json)
/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2/include/nlohmann/json.hpp
/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2/include/nlohmann/detail/output/serializer.hpp
/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2/include/nlohmann/detail/conversions/to_chars.hpp
/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2/include/nlohmann/detail/input/lexer.hpp
/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2/include/nlohmann/detail/iterators/iter_impl.hpp
/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2/include/nlohmann/detail/input/json_sax.hpp
/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2/include/nlohmann/detail/iterators/iteration_proxy.hpp
/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2/include/nlohmann/detail/input/parser.hpp
```

Note how absolute header paths are embedded into `nix` binary. It
happens via `__FILE__` macro expansion in C++ code.

```c++
//// in nlohmann_json/include/nlohmann.json.h
class basic_json {
    // ...
    template<typename T, typename... Args> static T* create(Args&& ... args)
    {
        // ...
        JSON_ASSERT(obj != nullptr);
        // ...
    }

//// in nlohmann_json/include/nlohmann/detail/macro_scope.hpp
// allow overriding assert
#if !defined(JSON_ASSERT)
    #include <cassert> // assert
    #define JSON_ASSERT(x) assert(x)
#endif

// in glibc/include/assert.h
# if defined __cplusplus
#  define assert(expr)                                                  \
     (static_cast <bool> (expr)                                         \
      ? void (0)                                                        \
      : __assert_fail (#expr, __FILE__, __LINE__, __ASSERT_FUNCTION))
// ...
```

In this case any code that happens to instantiate `basic_json::create()`
function will embed `__FILE__` definition as part of the `assert()` call
and will embed absolute path to the store.

Absolute path to the store will retain `nlohmann/json` in runtime closure.
It's a completely redundant runtime dependency.

## The workaround

Initially I though of using something like `nukeReferences`
file-mangling tool to wipe out unexpected references. But I was not sure
where should I plug this hammer: ideally any user of `nlohmann_json`
package should run it just in case. And patching files after-the-fast is
always prone to break something: be it broken file checksums, broken
sort ordering, unrelated string sharing due to identical code folding.

I wanted something milder: ideally tell `gcc` not to emit problematic
paths at all. And `gcc` provides exactly that mechanism!

`gcc` has a way to slightly mangle absolute paths used by `__FILE__` via
`-fmacro-prefix-map=old=new` set of options: <https://gcc.gnu.org/onlinedocs/gcc/Preprocessor-Options.html>.

It's main use case is to untangle final binaries from the temporary
directory sources are built against:
`-fmacro-prefix-map=/tmp/autogenerated/foo=/usr/src/foo`.

The problem feels vaguely similar: we want to avoid any mention of
source directories in the final output. I tried to inject
`-fmacro-prefix-map=` for every single build input used by `nixpkgs` as:

```diff
--- a/pkgs/build-support/cc-wrapper/setup-hook.sh
+++ b/pkgs/build-support/cc-wrapper/setup-hook.sh
@@ -65,15 +65,29 @@
 # function is guaranteed to be exactly the same.
 ccWrapper_addCVars () {
     # See ../setup-hooks/role.bash
-    local role_post
+    local role_post mangled_store map_flag var
     getHostRoleEnvHook

+    var=NIX_CFLAGS_COMPILE${role_post}
+
     if [ -d "$1/include" ]; then
-        export NIX_CFLAGS_COMPILE${role_post}+=" -isystem $1/include"
+        export $var+=" -isystem $1/include"
     fi

     if [ -d "$1/Library/Frameworks" ]; then
-        export NIX_CFLAGS_COMPILE${role_post}+=" -iframework $1/Library/Frameworks"
+        export $var+=" -iframework $1/Library/Frameworks"
+    fi
+
+    # Try hard to avoid hardcoding of -dev outputs via __FILE__.
+    # THe typical examples are: asserts in nlohmann_json leaking into
+    # nix executable closure, asserts from lttng-ust leaking into
+    # pipewire.
+    mangled_store=$(printf "%s" "$1" | sed -e "s|$NIX_STORE/[a-z0-9]\{32\}-|$NIX_STORE/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-|g")
+    map_flag="-fmacro-prefix-map=$1=$mangled_store"
+    # As it's a long flag try hard not to introduce duplicates as
+    # environment gets exhausted otherwise for large packages like qemu.
+    if [[ ${!var-} != *" $map_flag"* ]]; then
+        export $var+=" $map_flag"
     fi
 }
```

The change adds a bunch of compiler options on form of:
```
-fmacro-prefix-map=/nix/store/5xih6daf5g3hpa0wc5vs2cgrhakn4s0j-nlohmann_json-3.11.2=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-nlohmann_json-3.11.2
```

That way `__FILE__` values still have a reasonable form: we can see which
package they come from. But we can't use the path in any meaningful way
as they don't refer real files anymore. Does not look too bad.

I also had to avoid attempts at inserting multiple identical values into
the final `NIX_CFLAGS_COMPILE` variable. Otherwise some particularly
large packages like `qemu` exhaust all the environment space and fail to
run executables. We might want to do the same for the rest of variables:
we add most of the options options thrice to `NIX_CFLAGS_COMPILE`.

Proposed the workaround as <https://github.com/NixOS/nixpkgs/pull/255192>.

## The results

Did the workaround help? I placed the patch into `~/n` checkout of
`nixpkgs` repo.

```
$ LANG=C grep -R $(nix-build --no-link ~/n -A nlohmann_json) $(nix-build --no-link ~/n -A nix.out)
<empty>
```

Yay! At the very least we got rid of the unneeded reference.

`nix` was a good example of a superfluous and small harmless dependency.

Are there packages where this change would be more pronounced? My
ulterior motive was to fix something similar in `lttng-ust.dev`. That
one was not just a set of headers. It also contains python interpreter
as a dependency:

```
$ nix path-info -r $(nix-build '<nixpkgs>' -A lttng-ust.dev) |& unnix
/<<NIX>>/libunistring-1.1
/<<NIX>>/libidn2-2.3.4
/<<NIX>>/xgcc-12.3.0-libgcc
/<<NIX>>/glibc-2.37-8
/<<NIX>>/zlib-1.2.13
/<<NIX>>/sqlite-3.42.0
/<<NIX>>/numactl-2.0.16
/<<NIX>>/lttng-ust-2.13.1
/<<NIX>>/expat-2.5.0
/<<NIX>>/ncurses-6.4
/<<NIX>>/liburcu-0.14.0
/<<NIX>>/xz-5.4.4
/<<NIX>>/openssl-3.0.10
/<<NIX>>/bzip2-1.0.8
/<<NIX>>/libffi-3.4.4
/<<NIX>>/libxcrypt-4.4.36
/<<NIX>>/liburcu-0.14.0-dev
/<<NIX>>/gcc-12.3.0-libgcc
/<<NIX>>/gcc-12.3.0-lib
/<<NIX>>/readline-8.2p1
/<<NIX>>/bash-5.2-p15
/<<NIX>>/mailcap-2.1.53
/<<NIX>>/gdbm-1.23
/<<NIX>>/tzdata-2023c
/<<NIX>>/python3-3.10.12
/<<NIX>>/lttng-ust-2.13.1-bin
/<<NIX>>/lttng-ust-2.13.1-dev
```

One of the frequent users of `lttng-ust.dev` is `pipewire`:

```
$ nix path-info -r $(nix-build '<nixpkgs>' -A pipewire.out) | fgrep lttng
/nix/store/23jh1m6irhvr16zjmrvy2cnpgz7yi6gj-lttng-ust-2.13.1
/nix/store/xznqsvr1la1xnfnzia2yvnicfz03yjqb-lttng-ust-2.13.1-bin
/nix/store/fckk2ncjnxdw1xsx8v8rxjnmhldbx8pr-lttng-ust-2.13.1-dev
```

And I'm glad to announce that it's also gone with the patch above:

```
$ nix path-info -r $(nix-build ~/n -A pipewire.out) | fgrep lttng
/nix/store/bv3i3qjphzgfzmmdhws9nhwz76qscy61-lttng-ust-2.13.1
```

Here is the closure size difference:

Before:

```
$ nix path-info -rsSh $(nix-build '<nixpkgs>' -A pipewire.out) | nl | tail -n1
   219  /nix/store/ff0w34nr807in3b1swmqklxy9g9v5hg9-pipewire-0.3.79 1.4M  543.4M
```

After:

```
$ nix path-info -rsSh $(nix-build ~/n -A pipewire.out) | nl | tail -n1
   207  /nix/store/hl4dffvc73nsh3zfbji0y7h9lcnrk14b-pipewire-0.3.79 1.3M  452.0M
```

12 dependencies and ~90MB (`543.4` -> `452.0` reduction, ~20% of the whole
output) are just gone!

## Parting words

Looking at the runtime closure is always fun. There are many other
low-hanging fruits in `nixpkgs` to remove. Most of the time just adding
an extra `dev` output is enough to slim down the output.

`__FILE__` is a tricky macro that makes `nix` builds to leak out
unnecessary references into final closure.

`-fmacro-prefix-map=` seems to be a robust workaround for `__FILE__`
induced leaks. The flag is supported in both `gcc` and `clang` for quite
a while.

Even `nix` package itself had a redundant dependency in it's final
closure.

<https://github.com/NixOS/nixpkgs/pull/255192> should fix it in `nixpkgs`.

And for some packages like `pipewire` the closure size reduction is
substantial and is taking about 20% of all closure size. Not bad for a
single extra compiler option.

Have fun!
