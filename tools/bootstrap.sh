#!/usr/bin/env bash

# tools/bootstrap.sh [executable]
#
# Hazy makes its own compiler, and that compiler makes the compiler again.
# What the two make is the same, or this fails.
#
# The compiler is made into an executable (stage 2), and that makes the
# executable again (stage 3). The LLVM each was made from is compared.

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
