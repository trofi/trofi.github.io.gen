#!/bin/sh

setup_exe=./Setup
site_exe=dist/build/trofi.github.io.gen/trofi.github.io.gen

set -e

ghc --make -O2 Setup.hs -o "${setup_exe}" -dynamic
"${setup_exe}" configure --enable-executable-dynamic
"${setup_exe}" build

"${site_exe}" clean
"${site_exe}" watch "$@"
