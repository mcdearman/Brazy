#!/usr/bin/env bash

# tools/bootstrap.sh [executable]
#
# Hazy makes its own compiler into Javascript, and that compiler makes the
# compiler again. What the two make is the same, file for file, or this
# fails.

set -euo pipefail

# Get the executable or compile it ourselves
if [ "$#" -lt 1 ]; then
    tools/build.sh .bootstrap/dist
    HAZY=".bootstrap/dist/bin/hazy"
else
    HAZY="$1"
fi

out=".bootstrap/stages"
rm -rf "$out"
mkdir -p "$out"

# A compiler looks for its packages beside itself, wherever it was put.
cp -r "$(dirname "$(dirname "$HAZY")")/lib" "$out/lib"

echo "Stage 2: the compiler, made by the compiler that GHC built"
"$HAZY" -q -XStableImports compiler -o "$out/stage2"

# A program made of Haskell goes far deeper than Javascript is given room
# for. The stack only grows under a limit that is a number.
stack=4000000
if ! ulimit -s "$stack" 2> /dev/null; then
    stack="$(ulimit -H -s)"
    if [ "$stack" = unlimited ]; then
        stack=1000000
    fi
    ulimit -s "$stack" 2> /dev/null || stack="$(ulimit -s)"
fi
export BUN_JSC_maxPerThreadStackUsage=$((stack * 1000))

echo "Stage 3: the compiler, made by stage 2"
bun "$out/stage2/index.mjs" -q -XStableImports compiler -o "$out/stage3"

diff -r "$out/stage2" "$out/stage3"
echo "The stages agree: $(find "$out/stage3" -name '*.mjs' | wc -l) files"
