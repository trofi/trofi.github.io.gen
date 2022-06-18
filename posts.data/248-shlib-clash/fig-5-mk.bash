#!/usr/bin/env bash

{
printf "// generated with $ ./fig-5-mk.bash\n"
printf "digraph {\n"
printf "  node [shape=rect];\n"
{
lddtree $(which gdb) | while read l; do
  # name => /absolute/path ...
  set -- $l
  path=$3
  base=$(basename "$path")
  for dep in $(patchelf --print-needed "$path"); do
      printf "  \"%s\" -> \"%s\"\n" "$base" "$dep"
  done
done
# remove most frequent ones to avoid clutter)
} | egrep -v "libc.so.6|libgcc_s.so.1|libm.so.6|ld-linux-x86-64.so.2|libpthread.so.0|libdl.so.2|libstdc[+][+].so.6|librt.so.1" | sort -u
printf "}\n"
} > fig-5.dot
dot -Tsvg fig-5.dot > fig-5.svg
