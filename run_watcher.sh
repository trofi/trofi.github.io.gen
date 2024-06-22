#! /usr/bin/env nix-shell
#! nix-shell --pure
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.pandoc-types p.pandoc p.hakyll p.text])"
#! nix-shell -p gnuplot
#! nix-shell -p graphviz
#! nix-shell -p bash
#! nix-shell -i bash

export LANG=C.UTF-8

setup_exe=./Setup
site_exe=dist/build/trofi.github.io.gen/trofi.github.io.gen

set -e

ghc --make -O2 Setup.hs -o "${setup_exe}" -dynamic -fforce-recomp
"${setup_exe}" configure --enable-executable-dynamic --ghc-options=-threaded
"${setup_exe}" build

"${site_exe}" clean
"${site_exe}" watch "$@" +RTS -N$(nproc)
