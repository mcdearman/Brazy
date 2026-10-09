#!/usr/bin/env bash

# tools/test.sh [executable]

set -euo pipefail

# Clear out the old test temporaries
rm -rf .test
mkdir .test

# Get the executable or compile it ourselves
if [ "$#" -lt 1 ]; then
    tools/build.sh .test/dist
    HAZY=".test/dist/bin/hazy"
else
    HAZY="$1"
fi

# List the Haskell files under a directory, in a stable order
sources() {
    find "$1" -name '*.hs' | sort
}

# Run the bad parse tests
echo "=== Bad Parse ==="
for bad in $(sources test/bad/parse); do
    echo "Parsing $(basename $bad)"
    $HAZY --debug-fail "Expected" --parse "$bad"
done

# Run the bad resolve tests
echo "=== Bad Resolve ==="
for bad in $(sources test/bad/resolve); do
    base="${bad%.hs}"
    fail=$(cat "$base.test")
    $HAZY --debug-fail "$fail" --resolve "$bad"
done

# Run the bad check tests
echo "=== Bad Check ==="
for bad in $(sources test/bad/check); do
    base="${bad%.hs}"
    fail=$(cat "$base.test")
    $HAZY --debug-fail "$fail" --check "$bad"
done

# Run the good tests
echo "=== Good ==="
for good in $(sources test/good); do
    $HAZY "$good"
done

# Run the execution tests
echo "=== Execution ==="
for run_dir in test/run/*/; do
    run=$(basename "$run_dir")
    mkdir -p ".test/$run"
    echo "Running $run"
    $HAZY -q "test/run/$run/source" -o ".test/$run"
    node ".test/$run/index.mjs" > ".test/$run/result"
    diff "test/run/$run/result" ".test/$run/result"
done

# Run the execution tests on the AxCut machine
echo "=== Execution (AxCut) ==="
for run_dir in test/run/*/; do
    run=$(basename "$run_dir")
    echo "Running $run"
    $HAZY -q "test/run/$run/source" --run-axcut > ".test/$run/axcut"
    diff "test/run/$run/result" ".test/$run/axcut"
done

# Run the interactive session tests
echo "=== Interactive ==="
for repl_dir in test/repl/*/; do
    repl=$(basename "$repl_dir")
    mkdir -p ".test/repl/$repl"
    echo "Running $repl"
    $HAZY --repl < "test/repl/$repl/input" > ".test/repl/$repl/result" 2>&1
    diff "test/repl/$repl/result" ".test/repl/$repl/result"
done
