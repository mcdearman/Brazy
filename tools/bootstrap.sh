#!/usr/bin/env bash

# tools/bootstrap.sh [executable] [native | javascript | all]
#
# Hazy makes its own compiler, and that compiler makes the compiler again.
# What the two make is the same, or this fails.
#
#   native      The compiler is made into an executable (stage 2), and that
#               makes the executable again (stage 3). The LLVM each was made
#               from is compared. This is what is done when nothing is asked
#               for.
#   javascript  The compiler is made into Javascript (stage 2), and that, run
#               by Bun, makes the Javascript again (stage 3). Javascript is
#               on its way out, and this goes with it.
#   all         Both.

set -euo pipefail

# Get the executable or compile it ourselves
if [ "$#" -lt 1 ]; then
    tools/build.sh .bootstrap/dist
    HAZY=".bootstrap/dist/bin/hazy"
else
    HAZY="$1"
fi
what="${2:-native}"

out=".bootstrap/stages"
rm -rf "$out"
mkdir -p "$out"

# A compiler looks for its packages beside itself, wherever it was put.
cp -r "$(dirname "$(dirname "$HAZY")")/lib" "$out/lib"

# This runs in the stack a program is given, and in half of it. It used to
# ask for four gigabytes: one definition of the compiler went as deep as a
# list was long, and nothing else needed it.

if [ "$what" = javascript ] || [ "$what" = all ]; then
    echo "Stage 2: the compiler as Javascript, made by the compiler that GHC built"
    "$HAZY" -q -XStableImports compiler -o "$out/stage2"

    echo "Stage 3: the compiler as Javascript, made by stage 2 as Javascript"
    bun "$out/stage2/index.mjs" -q -XStableImports compiler -o "$out/stage3"

    diff -r "$out/stage2" "$out/stage3"
    echo "The stages agree: $(find "$out/stage3" -name '*.mjs' | wc -l) files"
fi

if [ "$what" = native ] || [ "$what" = all ]; then
    echo "Stage 2: the compiler as an executable, made by the compiler that GHC built"
    mkdir -p "$out/native2" "$out/native3"
    "$HAZY" -q -XStableImports compiler --native "$out/native2/hazy" 2> "$out/native2.log" \
        || { grep -v Warning "$out/native2.log"; exit 1; }

    echo "Stage 3: the compiler as an executable, made by stage 2"
    "$out/native2/hazy" -q -XStableImports compiler --native "$out/native3/hazy" 2> "$out/native3.log" \
        || { grep -v Warning "$out/native3.log"; exit 1; }

    cmp "$out/native2/hazy.ll" "$out/native3/hazy.ll"
    echo "The stages agree: $(wc -c < "$out/native3/hazy.ll") bytes of LLVM"

    # What stage 2 made is a compiler too.
    "$out/native3/hazy" --help > /dev/null
fi
