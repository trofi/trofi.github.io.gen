---
title: "Having fun with bash completion"
date: August 23, 2021
---

**2025 UPDATE**: I switched to [`fzf`](https://junegunn.github.io/fzf/)
(`Alt-C`) as a better alternative to the hack below.

I have a hard time navigating through directory structure in some
projects. I usually know the leaf directory name exactly, but not the
intermediate path.

Some examples are:

- `linux.git`: `i915` -> `drivers/gpu/drm/i915`
- `gentoo.git`: `xmms2` -> `media-sound/xmms2`
- `nixpkgs.git`: `re2c` -> `pkgs/development/tools/parsing/re2c`

First two I somehow tolerated for a while and was able to get away with
an equivalent of `cd */xmms2`. But for `nixpkg` it became too much to
type.

First, I wrote a one-liner to `cd` right into a subdirectory if it's a
unique directory across all subdirectories:

``` bash
cdc() {
    if [[ -d $1 ]]; then
        cd "$1"
        return
    fi

    local candidates=()
    # Be careful to handle directories with whitespace
    # and special characters that could break tokenization
    while IFS= read -r -d $'\0' d; do
        [[ $d == '.' ]] && continue
        candidates+=("${d#./}")
    done < <(find -path "*/$2" -type d -print0 2>/dev/null)

    if [[ ${#candidates[@]} -eq 1 ]]; then
        cd "${candidates[@]}"
        return
    fi
    echo "ERROR: cdc: '$1' is ambiguous (${#candidates[@]}) entries. Can't cd."
    return 1
}
```

Example usage session:

``` 
~ $ cdc re2c
ERROR: cdc: 're2c' is ambiguous (8) entries. Can't cd.

~ $ cd portage/gentoo
~/portage/gentoo $ cdc re2c
~/portage/gentoo/dev-util/re2c $
```

Looks straightforward. Then I thought of hooking up bash completion to
avoid typing full intermediate directory. And to see interactively what
these ambiguities are. For example, in case of
`pkgs/development/python-modules/importlib-metadata` I'd like to
avoid typing `importlib-metadata` while being able to get to it
quicker.
Apparently, `bash` does not require bash completion to be an exact
prefix for something and allows for any arbitrary substitution!
Here is a silly example to get basics of bash completion:

``` bash
# complete-hia.bash
# $1 - 'hia'
# $2 - the word being completed
# $3 - the word before completion
_complete_hia() {
    # generic completion results:
    COMPREPLY=(
        # just generate 5 random entries
        $RANDOM
        $RANDOM
        $RANDOM
        $RANDOM
        $RANDOM
        # and a fancy output
        "Hia! Here is your full arg list: '$*'"
    )

    # let's do something special on exact match
    if [[ $2 == secret ]]; then
        COMPREPLY=( "YOU GOT IT!")
    fi
}
complete -F _complete_hia hia
```

Example session:

``` 
$ source complete-hia.bash
$ hia a<TAB>
1476    29762
14984   31708
15184   Hia! Here is your full arg list: 'hia a hia'
$ hia a<TAB>
22726  3483
24271  8982
32492  Hia! Here is your full arg list: 'hia a hia'
$ hia is it secret<TAB>
$ hia is it YOU GOT IT!
```

There are many minor caveats like automatic prefix expansion when all
alternatives match (make sure to check `compgen` documentation).

Let's try arbitrary directory completion for `cdc` command introduced
above.

``` bash
_cdc() {
    local d candidates=()
    while IFS= read -r -d $'\0' d; do
        [[ $d == '.' ]] && continue
        candidates+=("${d#./}")
    done < <(find -path "*/$2" -type d -print0 2>/dev/null)
    COMPREPLY=(
        # multiple candidates, don't match on prefix. Just dump all.
        # Also always quote output
        "${candidates[@]@Q}"
    )
    if [[ ${#candidates[@]} -gt 1 ]]; then
        # If there is ambiguity do not mangle original argument
        COMPREPLY+=( "${2}" )
    fi
}

complete -F _cdc cdc
```

Here is the example session:

``` 
~ $ cdc *rtlib-*
'dev/git/nixpkgs/pkgs/development/python-modules/importlib-metadata'
'dev/git/nixpkgs/pkgs/development/python-modules/importlib-resources'
*rtlib-*

~ $ cdc *rtlib-m*<TAB>

~ $ cdc 'dev/git/nixpkgs/pkgs/development/python-modules/importlib-metadata'

~ $ cdc curseofwar<TAB>
curseofwar
'dev/git/nixpkgs/pkgs/games/curseofwar'
'portage/slyfox-gentoo/games-strategy/curseofwar'
```

I made trailing globs to be clunky to use on purpose as I use exact
match most of the time. One could cook a version with many enhancements
like `find`'s case-insensitive match or do something smarter around
completion quoting.

More info is at [Programmable Completion
Builtins](https://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html).

Have fun!
