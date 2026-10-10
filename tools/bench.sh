#!/usr/bin/env bash

# tools/bench.sh [executable]
#
# Compile each program in bench/ natively, with and without building values
# in the place of ones just taken apart, and say what each run allocated.
# Whole programs are compiled, so that the setting reaches the packages too.

set -euo pipefail

HAZY="${1:-.dist/bin/hazy}"
OUT=.test/bench
mkdir -p "$OUT"

for bench_dir in bench/*/; do
    bench=$(basename "$bench_dir")
    $HAZY -q "bench/$bench" --native "$OUT/$bench" --whole
    $HAZY -q "bench/$bench" --native "$OUT/$bench-no-reuse" --whole --no-reuse
    for variant in "$bench" "$bench-no-reuse"; do
        start=$(date +%s%N)
        result=$(HAZY_GC_STATS=1 "$OUT/$variant" 2> "$OUT/$variant.stats")
        end=$(date +%s%N)
        printf '%-20s %6d ms  result %s\n    %s\n' "$variant" $(((end - start) / 1000000)) "$result" "$(cat "$OUT/$variant.stats")"
    done
done
