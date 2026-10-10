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
    bun ".test/$run/index.mjs" > ".test/$run/result"
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

# Run the execution tests natively. Collections are made often, and at each
# one every count of references is worked out again and compared, so that a
# reference that is lost or given up twice is found where it happens.
echo "=== Execution (native) ==="
export HAZY_GC_VERIFY=1
export HAZY_GC_THRESHOLD=4096
for run_dir in test/run/*/; do
    run=$(basename "$run_dir")
    mkdir -p ".test/native"
    echo "Running $run"
    $HAZY -q "test/run/$run/source" --native ".test/native/$run"
    ".test/native/$run" > ".test/native/$run.result"
    diff "test/run/$run/result" ".test/native/$run.result"
done

# Values that refer to themselves are freed by tracing, not by counting
echo "Checking that cycles are collected"
HAZY_GC_STATS=1 ".test/native/cycles" 2>&1 > /dev/null | grep -q "freed by tracing: [1-9]"

# Run one test natively as a whole program, with the packages compiled again
echo "=== Execution (native, whole program) ==="
echo "Running hello"
$HAZY -q "test/run/hello/source" --native ".test/native/hello-whole" --whole
".test/native/hello-whole" > ".test/native/hello-whole.result"
diff "test/run/hello/result" ".test/native/hello-whole.result"

# Build each test package, and run the program that uses it both ways
echo "=== Packages ==="
for package_dir in test/package/*/; do
    package=$(basename "$package_dir")
    out=".test/package/$package"
    mkdir -p "$out"
    echo "Running $package"
    $HAZY -q "test/package/$package/library" -o "$out/library" --pack
    $HAZY -q "test/package/$package/library" --native-object "$out/library/native.o"
    $HAZY -q "test/package/$package/source" --package "./$out/library" -o "$out/javascript"
    bun "$out/javascript/index.mjs" > "$out/javascript.result"
    diff "test/package/$package/result" "$out/javascript.result"
    $HAZY -q "test/package/$package/source" --package "./$out/library" --native "$out/native"
    "$out/native" > "$out/native.result"
    diff "test/package/$package/result" "$out/native.result"
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
