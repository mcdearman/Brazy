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
        # Run it once unmeasured: the first run of a new executable is slow
        # on macOS, which checks it over.
        "$OUT/$variant" > /dev/null
        # The time the program itself took, as the shell measures it
        TIMEFORMAT=%R
        seconds=$({ time HAZY_STATS=1 "$OUT/$variant" > "$OUT/$variant.result" 2> "$OUT/$variant.stats"; } 2>&1)
        printf '%-20s %6ss  result %s\n    %s\n' "$variant" "$seconds" "$(cat "$OUT/$variant.result")" "$(cat "$OUT/$variant.stats")"
    done
done
