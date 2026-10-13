#!/usr/bin/env bash

# tools/bootstrap.sh [executable] [javascript | native | all]
#
# Hazy makes its own compiler, and that compiler makes the compiler again.
# What the two make is the same, or this fails.
#
#   javascript  The compiler is made into Javascript (stage 2), and that, run
#               by Bun, makes the Javascript again (stage 3). This is what is
#               done when nothing is asked for.
#   native      The compiler is made into an executable (stage 2), which
#               makes the Javascript, the same as stage 1 made.
#   llvm        As native, and then the executable makes the LLVM of itself,
#               the same as stage 1 made it from. This takes a great deal of
#               memory.
#   all         All of them.

set -euo pipefail

# Get the executable or compile it ourselves
if [ "$#" -lt 1 ]; then
    tools/build.sh .bootstrap/dist
    HAZY=".bootstrap/dist/bin/hazy"
else
    HAZY="$1"
fi
what="${2:-javascript}"

out=".bootstrap/stages"
rm -rf "$out"
mkdir -p "$out"

# A compiler looks for its packages beside itself, wherever it was put.
cp -r "$(dirname "$(dirname "$HAZY")")/lib" "$out/lib"

echo "Stage 2: the compiler as Javascript, made by the compiler that GHC built"
"$HAZY" -q -XStableImports compiler -o "$out/stage2"

# This runs in the stack a program is given, and in half of it. It used to
# ask for four gigabytes: one definition of the compiler went as deep as a
# list was long, and nothing else needed it.

if [ "$what" = javascript ] || [ "$what" = all ]; then
    echo "Stage 3: the compiler as Javascript, made by stage 2 as Javascript"
    bun "$out/stage2/index.mjs" -q -XStableImports compiler -o "$out/stage3"

    diff -r "$out/stage2" "$out/stage3"
    echo "The stages agree: $(find "$out/stage3" -name '*.mjs' | wc -l) files"
fi

if [ "$what" = native ] || [ "$what" = llvm ] || [ "$what" = all ]; then
    echo "Stage 2: the compiler as an executable, made by the compiler that GHC built"
    mkdir -p "$out/bin"
    "$HAZY" -q -XStableImports compiler --native "$out/bin/hazy" 2> "$out/native2.log" \
        || { grep -v Warning "$out/native2.log"; exit 1; }

    echo "Stage 3: the compiler as Javascript, made by stage 2 as an executable"
    "$out/bin/hazy" -q -XStableImports compiler -o "$out/stage3-native"

    diff -r "$out/stage2" "$out/stage3-native"
    echo "The stages agree: $(find "$out/stage3-native" -name '*.mjs' | wc -l) files"

fi

if [ "$what" = llvm ] || [ "$what" = all ]; then
    echo "Stage 3: the compiler as LLVM, made by stage 2 as an executable"
    "$HAZY" -q -XStableImports compiler --debug-llvm > "$out/stage2.ll" 2> /dev/null
    "$out/bin/hazy" -q -XStableImports compiler --debug-llvm > "$out/stage3.ll" 2> /dev/null

    cmp "$out/stage2.ll" "$out/stage3.ll"
    echo "The stages agree: $(wc -c < "$out/stage3.ll") bytes of LLVM"
fi
