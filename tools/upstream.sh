#!/usr/bin/env bash

# tools/upstream.sh [executable]
#
# Brazy compiles Hazy, the compiler it is a fork of, from Hazy's own source.
# The compiler that makes then builds Hazy's packages and runs Hazy's tests.
#
# Hazy's source is fetched as it was at one commit and is changed only by
# tools/upstream.patch: the few places where it does what Brazy cannot do
# yet. What is in that file is what is left to do, and the day it is empty
# Brazy compiles Hazy as Hazy is written.

set -euo pipefail

if [ "$#" -lt 1 ]; then
    tools/build.sh .upstream/dist
    HAZY="$PWD/.upstream/dist/bin/hazy"
else
    HAZY="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
fi

COMMIT=ad08e01
here="$PWD"
work="$PWD/.upstream/hazy"

if [ ! -d "$work/.git" ]; then
    rm -rf "$work"
    mkdir -p "$work"
    git clone --quiet https://github.com/Superstar64/Hazy.git "$work"
fi
git -C "$work" fetch --quiet origin
git -C "$work" checkout --quiet --force "$COMMIT"
git -C "$work" clean --quiet -fdx
if [ -s "$here/tools/upstream.patch" ]; then
    git -C "$work" apply "$here/tools/upstream.patch"
    echo "Changed for what Brazy cannot do yet: $(grep -c '^diff ' "$here/tools/upstream.patch") files"
fi

cd "$work"
DIST="$work/.dist"
PACKAGE="$DIST/lib/hazy/packages"
mkdir -p "$DIST/bin" "$PACKAGE"

echo "Hazy's compiler, made by Brazy"
"$HAZY" -q -XStableImports compiler -o "$DIST/bin"

# What Brazy made is Javascript. Hazy's tools ask for an executable, and
# this is one: a program made of Haskell wants a deeper stack than
# Javascript is given, which only grows under a limit that is a number.
cat > "$DIST/bin/hazy" << WRAPPER
#!/usr/bin/env bash
stack=4000000
if ! ulimit -s "\$stack" 2> /dev/null; then
    stack="\$(ulimit -s)"
    if [ "\$stack" = unlimited ]; then stack=1000000; fi
fi
export BUN_JSC_maxPerThreadStackUsage=\$((stack * 1000))
exec bun "$DIST/bin/index.mjs" "\$@"
WRAPPER
chmod +x "$DIST/bin/hazy"

# The rest is Hazy's tools/build.sh after it has built the compiler.
echo "Hazy's packages, made by that compiler"
mkdir -p "$PACKAGE/hazy-internal/header/Hazy" "$PACKAGE/hazy-internal/artifacts/Hazy"
cp runtime/package "$PACKAGE/hazy-internal/package"
SOURCE="Hazy/Helper Hazy/Prelude Hazy/PreludeList Hazy/PreludeText Hazy/PreludeIO Hazy/PreludeChar Hazy/PreludeRatio Hazy/PreludeNumeric Hazy/PreludeMonad Hazy/PreludeString"
cp runtime/header/Hazy.hs "$PACKAGE/hazy-internal/header/Hazy.hs"
for FILE in $SOURCE; do
    cp "runtime/source/$FILE.hs" "$PACKAGE/hazy-internal/header/$FILE.hs"
done
cp runtime/javascript/Hazy.mjs "$PACKAGE/hazy-internal/artifacts/Hazy.mjs"
"$DIST/bin/hazy" -q --bare -c -I runtime/header runtime/source -o "$PACKAGE/hazy-internal/artifacts" $(cat runtime/flags)
"$DIST/bin/hazy" -q --pack --bare-runtime library/base/source -o "$PACKAGE/base" $(cat library/base/flags)

# Hazy's tests run what they make with Node. Bun is what is here.
echo "Hazy's tests, with that compiler"
sed 's/^    node /    bun /' tools/test.sh > "$work/.test.sh"
bash "$work/.test.sh" "$DIST/bin/hazy" > "$work/.test.log" 2>&1 || { tail -30 "$work/.test.log"; exit 1; }
echo "Hazy's tests pass: $(grep -c '^Running' "$work/.test.log") programs run"
