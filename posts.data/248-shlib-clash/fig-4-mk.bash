#!/usr/bin/env bash

{
printf "// generated with $ ./fig-4-mk.bash\n"
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
} | sort -u
printf "}\n"
} > fig-4.dot
dot -Tsvg fig-4.dot > fig-4.svg
