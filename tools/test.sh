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

# Run the bad resolve tests that take more than one module
for bad_dir in test/bad/modules/*/; do
    fail=$(cat "$bad_dir/test")
    $HAZY --debug-fail "$fail" --resolve "$bad_dir/source"
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

# Run the execution tests on the AxCut machine. It is what says a program
# is compiled rightly: it runs AxCut as AxCut is written, with nothing made
# of it. The programs were run as Javascript for that, before there was
# anything else to run them as.
echo "=== Execution (AxCut) ==="
for run_dir in test/run/*/; do
    run=$(basename "$run_dir")
    mkdir -p ".test/$run"
    echo "Running $run"
    $HAZY -q "test/run/$run/source" --run-axcut > ".test/$run/axcut"
    diff "test/run/$run/result" ".test/$run/axcut"
done

# Run the execution tests natively. They are compiled to be checked: at the
# start of every function every count of references is worked out again and
# compared, so that a reference that is lost or given up twice is found
# where it happens. A block that nothing reaches is found as well, so no
# program here makes values that hold themselves.
echo "=== Execution (native) ==="
# The programs are checked as they run: every count of references is worked
# out again and compared. That is asked of the programs and not of the
# compiler, which may be a program that Hazy made too: it keeps things in
# places that refer to each other, and those are never given up.
for run_dir in test/run/*/; do
    run=$(basename "$run_dir")
    mkdir -p ".test/native"
    echo "Running $run"
    $HAZY -q "test/run/$run/source" --native ".test/native/$run" --checked
    HAZY_CHECK=1 ".test/native/$run" > ".test/native/$run.result"
    diff "test/run/$run/result" ".test/native/$run.result"
done

# A value that refers to itself and cannot go to the top is warned of
echo "Checking that a value that is no longer shared with itself is warned of"
$HAZY -q "test/run/cycles/source" --native ".test/native/cycles" --checked 2> ".test/native/cycles.warnings"
grep -q "refers to itself" ".test/native/cycles.warnings"

# Run one test natively as a whole program, with the packages compiled again
echo "=== Execution (native, whole program) ==="
echo "Running hello"
$HAZY -q "test/run/hello/source" --native ".test/native/hello-whole" --whole --checked
HAZY_CHECK=1 ".test/native/hello-whole" > ".test/native/hello-whole.result"
diff "test/run/hello/result" ".test/native/hello-whole.result"

# Build each test package, and run the program that uses it
echo "=== Packages ==="
for package_dir in test/package/*/; do
    package=$(basename "$package_dir")
    out=".test/package/$package"
    mkdir -p "$out"
    echo "Running $package"
    $HAZY -q "test/package/$package/library" -o "$out/library" --pack
    $HAZY -q "test/package/$package/library" --native-object "$out/library/native.o"
    $HAZY -q "test/package/$package/source" --package "./$out/library" --run-axcut > "$out/axcut.result"
    diff "test/package/$package/result" "$out/axcut.result"
    $HAZY -q "test/package/$package/source" --package "./$out/library" --native "$out/native" --checked
    HAZY_CHECK=1 "$out/native" > "$out/native.result"
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
