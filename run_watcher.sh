#!/bin/sh

export LANG=C.UTF-8

rm -rfv out/

while :
do
    cargo run --release
    inotifywait -r -e close_write .
done
