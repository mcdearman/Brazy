# Brazy

> **This is a fork.** Brazy is Chris McDearman's testing fork of
> [Hazy](https://github.com/Superstar64/Hazy), the Haskell compiler by
> Freddy (Superstar64). It is a place to try ideas and experiments before any
> of them are offered to Hazy. The fork has Freddy's blessing, but none of the
> work here is necessarily his. It keeps Hazy's git history and Hazy's
> licenses. The plan for the fork is in [ROADMAP.md](ROADMAP.md). The rest of
> this file is Hazy's README, with additions for what the fork has added;
> where it says "I", that is Hazy's author.
>
> **AI use.** Most of the work that is in this fork and not in Hazy was written
> by an AI coding assistant (Anthropic's Claude, through Claude Code) under
> Chris's direction. This includes the sequent calculus and AxCut
> intermediate languages, the LLVM backend, the Rust runtime, the REPL and the
> roadmap. Those commits carry a `Co-Authored-By: Claude` line. Freddy's
> blessing is for the fork as a whole: he has not reviewed this code, and it
> should not be taken as his work or as approved by him. It is tested by
> running programs on a reference machine for AxCut and as native code, but
> it is experimental, so read it with care before relying on it.

# The Hazy Haskell Compiler

Hazy is a Haskell compiler where I plan to experiment on making Haskell more
performant.

As of now the project is in a very early stages. The compiler currently unable
to bootstrap itself, the standard library isn't implemented yet, some major
Haskell features are missing, only a test Javascript backend is implemented.

# TODOs

- Use `UnorderedRecords` all throughout compiler
- Add top level documentation to all modules

# Missing

- Deriving (Partial)
- GADTs (Partial)
- Polymorphic Components
- Strict Functions
- Some Builtin Instances

# Milestones Checklist

- [ ] Standard Library
- [ ] Bootstrapping

# Quickstart

Ensure that GHC 9.14 and Cabal 3.16 are downloaded. Other GHC and Cabal versions
will probably work too but aren't tested regularly. Programs are compiled to
native code, which wants `clang` and `cargo`.

```sh
# (optional) Tell Cabal to use GHC 9.14
cabal configure --with-compiler=ghc-9.14

# Run the build script
tools/build.sh .dist

# Add the compiler to the path for testing
export PATH=.dist/bin:$PATH

# Compile hello world to a native executable (this wants clang and cargo)
hazy test/run/hello/source --native .hello/hello

# Run hello world
.hello/hello
```

# Windows

Everything here works on Windows 10 or later: the compiler, the interactive
session and native code. The build and the tests are shell scripts, so they
are run from Git Bash, with `make`, GHC, Cabal, Bun, `cargo` and `clang` on
the path. `clang` is the one that comes with LLVM for Windows and uses
Microsoft's linker, so Visual Studio's build tools have to be installed too.

A native program is `PATH.exe`. A call passes four arguments in registers
there and the rest through memory, where the other systems pass eight,
because LLVM cannot guarantee a tail call that passes arguments on the stack
on Windows.

# Native Code

`hazy SOURCE --native PATH` compiles a program to a native executable. It
writes LLVM IR to `PATH.ll` and has `clang` compile it and link it with the
runtime in `runtime/native`, which is written in Rust. Both `clang` and
`cargo` are needed. The tests are run for every change on arm64 macOS, on
x86-64 Linux and on x86-64 Windows.

```sh
make run ARGS="test/run/hello/source --native .hello"
./.hello
```

Only the program's own modules are compiled. The packages it loads were
compiled when they were made, and are linked in. `--whole` compiles everything
together instead.

Memory is freed by counting references, and by nothing else: there is no
garbage collector. Counting cannot free a value that holds itself, so the
compiler makes none. A value in a `let` or a `where` that refers to itself
goes to the top of the program if it uses nothing of where it is. If it does,
it is worked out again each time it refers to itself, and the compiler warns
of it: the value is no longer shared with itself, which can cost a great deal
of time. Defining it at the top of the module keeps it shared.

Two environment variables are read by a compiled program:

* `HAZY_STATS`: at exit, print how many blocks were allocated, how many were
  built in the place of another, how many calls lent what they passed, and
  how many blocks were freed.
* `HAZY_CHECK`: at exit, work every count of references out again, and stop
  if one is wrong or if a block is left that nothing reaches. A program
  compiled with `--checked` does this at the start of every function as well,
  or, given a number, after every so many allocations. That costs a test at
  the start of every function, so it is for testing the compiler.

A value is built in the place of one that was just taken apart, when that was
the last reference to it. `--no-reuse` turns this off, and `tools/bench.sh`
compiles the programs in `bench` both ways and prints what each allocated.

# Interactive Session

`hazy --repl` starts a prompt. An expression is evaluated: an `IO` action is
run and any other value is shown. Declarations are added to the session, and
declaring a name again replaces the entry that declared it.

```
> double :: Int -> Int
  double x = x + x

> double 21
42
```

Enter submits an entry once it parses. An entry that does not parse yet is
carried on to the next line, and a blank line ends it. Alt-Enter or Ctrl-J
starts a new line whatever the entry looks like. `:help` lists the commands,
which include `:load PATH`, `:sequent EXPR` and `:axcut EXPR`.

`:ir EXPR` opens a full-screen viewer on an expression, or on a definition if
it is given the name of one, with a page for each form the compiler puts it
in: Core, the sequent calculus, AxCut and LLVM IR. Tab moves to the next
page, Shift-Tab to the one before, the arrow keys and Page Up and Page Down
scroll, and `q` closes it. The viewer is the small library in `viewer`, which
wraps the `brick` package.

Entries are compiled as a whole module each time and run on the AxCut machine,
so the session is subject to everything the compiler is.

The line editor is [lambline](https://github.com/mcdearman/lambline), which
`cabal.project` fetches at a pinned commit.

# Errors

Errors are shown against the source they are in. The drawing is done by the
small library in `diagnostics`, which wraps the `diagnose` package and knows
nothing about the compiler. `Error.report` turns a `HazyError` into one of its
reports.

# Running From Source

The `Makefile` in the project root builds the compiler and its packages when
their sources have changed, and runs them:

```sh
make                 # build
make run             # start an interactive session
make run ARGS="test/run/hello/source --run-axcut"
make test            # run the tests
```

# Packages

Hazy has a bare minimum package system at the moment. A package is a directory
with these in it:
* `$PACKAGE/package`: This contains the package metadata. As of now, it only
   includes the default extensions and the list of Haskell files in the package.
* `$PACKAGE/header`: This contains the compiler generated header files that are
   not looked at for code generation. As of now, these files are just copies of
   their original source files.
* `$PACKAGE/native.o`: The package compiled to native code, which a program
   that uses the package is linked with.

To generate a package, just compile a set of modules and pass `--pack`.

For example:
```sh
hazy Module.hs modules/ -o mypackage --pack
```
This will generate a package into a directory with the specified name.

The native object is made separately and kept in the package as `native.o`:
```sh
hazy Module.hs modules/ --native-object mypackage/native.o
```

To load a package, pass `--package $NAME` or `--package $PATH`. `--package` will
try to load a package from the system package directory if the argument doesn't
start with `./` or `/` (and `.\\`, full drives paths on Windows).

For example:
```sh
hazy --package pkg ... # system package
hazy --package ./mypackage ... # specific package
```
This causes of all of the packages modules to be added to the module list and,
when generating a full executable, the package's native object to be linked
into the final program.

As of now, compiled packages have to be manually moved to into system packages
folder (`$DIST/lib/hazy/packages`) to be easily loadable by `--package`.

By default hazy will load `hazy-internal` and `base` from it's system packages
directory. This can be disabled with `--bare` and `--bare-runtime` respectively.

# Extensions
Hazy implements original extensions alongside some GHC extensions.
Note that you can only toggle the only extensions that aren't fully backwards
compatible.

## Custom Extensions

### Constructor Fields
* Pragma: `ConstructorFields`
* Toggleable: True

This is Hazy's take on `DisambiguateRecordFields`. This extension causes
constructor fields to be associated to the constructor and thus may be accessed
without needing to import said fields.

For example, this would now be valid:
```haskell
module A (Point (Point)) where

data Point = Point { x, y :: Int}
```
```haskell
module B where
import A (Point (Point))

construct = Point { x = 1, y = 2 }
```

This extension applies per constructor, so even modules that have this extension
disabled are still affected.

Additionaly, the `CONSTRUCTORFIELDS` pragma can enable this per constructor.

```haskell
data Multiple = Single Int | Many { x, y :: Int }

{-# ConstructorFields Many #-}
```

### Unordered Fields
* Pragma: `UnorderedRecords`
* Toggleable: True

This causes record declaration is this module to be unordered. This means
that they cannot be accessed with order dependent pattern matching.

Consider this example:
```haskell
data Point = Point { x, y :: Double }

dot (Point x y) = x + y -- #1
dot' Point {x, y} = x + y -- #2
```
Here, #1 is order dependent so it would be rejected while #2 is order independent
so it would be accepted.

This extension applies per constructor, so even modules that have this extension
disabled are still affected.

Additionaly, the `UNORDEREDRECORDS` pragma can enable this per constructor.

For example:
```haskell
data Multiple = Single Int | Many { x, y :: Int }

{-# UnorderedRecords Many #-}
```

### Stable Imports
* Pragma: `StableImports`
* Toggleable: True

This extension requires this property to hold:

    Your imported modules may freely add new symbols without causing overlapping symbols.

This boils down to requiring all imports either use import lists or non
overlapping qualified imports.

Consider this example:
```haskell
import A                -- #1
import B (x, Y(..))     -- #2
import qualified C      -- #3
import qualified D      -- #4
import qualified E as D -- #5
```

Import #1 is rejected because it imports everything. Import #4, #5 are rejected
because they overlap with import lists. Import #2 is okay because it uses an
import list. Import #3 is okay because it's the only qualified module in it's
namespace.

Note that exporting new constructors is considered an API break, so importing
all constructors of a type is still allowed.

This can be disabled with `{-# LANGUAGE NoStableImports #-}` or
`{-# LANGUAGE Haskell2010 #-}`.

### Of Guard Blocks
* Pragma: `OfGuardBlocks`
* Toggleable: False

Guard blocks for functions and multiway if blocks may use the `of` keyword
instead of `|`. This simply treats the block as newline intended statement.

Consider this example:
```haskell
addFail maybe1 maybe2
  | Just value1 <- maybe1
    Just value2 <- maybe2
  = value1 + value2 
  | otherwise = 0
```

This may be written like:
```haskell
addFail maybe1 maybe2
  of
    Just value1 <- maybe1
    Just value2 <- maybe2
    value1 + value2
  of
    0
```

### Extended Local Declarations
* Pragma: `ExtendedLocalDeclarations`
* Toggleable: False

Local `let` and `where` can include import, data and class declarations.

For example this code is now possible:
```haskell
sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy by = map runBy . sort . map By where
  import Data.List (sort)
  newtype By = By { runBy :: a }
  instance Eq By where
    a == b
      | EQ <- compare a b = True
      | otherwise = False
  instance Ord By where
    compare (By a) (By b) = by a b
```

### Extended Function Bindings 
* Pragma: `ExtendedFunctionBindings`
* Toggleable: False

Function bindings are not requried to be contiguous and they may have different
number of clauses.

For example, this is legal:
```haskell
notJust (Just bool) = Just (not bool)
id x = x
notJust = id
```

Here, both `notJust` are combined into a single function and the second case
of `notJust` is eta expanded.

### Extended Import Declarations
* Pragma: `ExtendedImportDeclarations`
* Toggleable: False

Import declarations may appear anywhere inside a module.

### Levity Polymorphic Fields
* Pragma: `LevityPolymorphicFields`
* Toggleable: False

Fields can be parameterized by their strictness (also called levity). A field
can prefixed with `~!s` to signify that it has levity `s`. Here, `s` must have
kind `Levity`. The main elements of `Levity` are `Lazy` and `Strict` and
they correspond to lazy and strict fields.

For example, here's a pair that could be lazy or strict:
```haskell
import Data.Kind (Type, Levity, Strict, Lazy)

type Pair :: Levity -> Type -> Type -> Type
data Pair s a b = Pair ~!s a ~!s b

lazyPair :: a -> b -> Pair Lazy a b
lazyPair a b = Pair a b

strictPair :: a -> b -> Pair Strict a b
strictPair a b = Pair a b
```

Like GHC's levity polymorphism, when a constructor is called or pattern matched
against, all it's fields must have monomorphic levity variables.

Consider this example:
```haskell
polyPair :: a -> b -> Pair s a b
polyPair a b = Pair a b

idPair :: Pair s a b  -> Pair s a b
idPair p = p
```
Here, `polyPair` isn't legal because the compiler don't know the levity of `s`
for the constructor. However `idPair` is okay, because the constructors are
never called or pattern matched against.

This restriction is only applied when the type variable is unknown at runtime.
Since type class default methods are always inlined, a type class's type
variable is allowed to be levity polymorphic.

For example, this is legal:
```haskell
class IntPair s where
  intPair :: Pair s Int Int
  intPair = Pair 0 0

instance IntPair Lazy
instance IntPair Strict
```

### Permissive Updates
* Pragma: `PermissiveUpdates`
* Toggleable: True

This changes the behavior of record updates. Normally in Haskell 2010, when
update matches on a constructor that doesn't have all the selections the result
is bottom. With permissive updates, any missing selections are ignored.

Consider this example:
```haskell
data X = A { x :: Int } | B { y :: Char }

updated =
  let base = A { x = 1 }
  in base { y = 'a' }
```

With permissive updates off, `updated` would be bottom. With permissive updates,
on, `updated` would be `A { x = 1 }`.

### Hygienic Hiding
* Pragma: `HygienicHiding`
* Toggeable: `True`

Haskell 2010 specifics that `hiding` declarations that hide a type constructor
must also hide data constructors. This is counter intuitive to what one would
expect. With hygienic hiding enabled, hazy instead only hides the type
constructor.

For example:
```haskell
import Prelude hiding ( Just )
```

Will hide the `Just` constructor in Haskell 2010, but this wouldn't hide
anything with hygienic hiding enabled.

Hiding constructors now has to properly mirror the explicit import list syntax:
```haskell
import Prelude hiding ( Maybe (Just) )
```

## GHC Extensions
### Scoped Type Variables
* Pragma: `ScopedTypeVariables`
* Toggleable: False

Hazy only implements the scoped type variables with type signaturues.
It does not implement pattern type signatures.

### Polymorphic Components
* Pragma: `PolymorphicComponents`

This is a subset of GHC's `RankNTypes`.

Planned.

### GADTs
Pragma: `GADTs`

A type can be declared with `where`, each constructor given with its whole
type. A constructor may make the type at arguments of its own choosing, and
matching on it then tells what the type variables of what was matched are,
for the branch the match leads to:

```haskell
data Expr a where
  Number :: Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval (Number n) = n
eval (If c t e) = if eval c then eval t else eval e
```

What is not there yet:

* A constructor that requires something of its types (`Show a => a -> T`).
* Records in a type declared with `where`, and deriving for one.
* The type of what is matched has to be known where it is matched, from a
  signature. Hazy does not work it out from the branches.
* A type variable that a constructor hides is taken for an unknown type, so a
  program that lets one get out is not refused.
* A branch that can never be reached is not refused either.

### Implicit Prelude
* Pragma `ImplicitPrelude`
* Toggleable: True

The implicit prelude can be turn off like in GHC. This has the same effect of
as `import Prelude()`.

### Lambda Case
* Pragma: `LambdaCase`
* Toggleable: False

Lambda case syntax is supported.

### MultiwayIf
* Pragma: `MultiwayIf`
* Toggleable: False

Multiway if syntax is supported.

### Empty Case
* Pragma: `EmptyCase`
* Toggleable: False

Empty case syntax is supported.

### Data Kinds
* Pragma: `DataKinds`
* Toggleable: False

Data kinds are supported. However, The scope of constructors does not pollude
the scope of types. This means that type level data constructors must be ticked.

If you want unticked operators, it's recommended you use type synonyms.
```haskell
data Lifted = A | B

type A = 'A
type B = 'B
```

Additionally, types have two universe levels: `type Small` and `type Large`.
Large types cannot be abstracted over or used in data types.

Consider this example:
```haskell
type BoolList = '[ 'True, 'False ] -- #1
type TypeList = '[ Int, Char ] -- #2
```

Here, #1 is okay because it's a just a lifted list of booleans.
However, #2 is a illegal because lists expect their elements to be `Type`s and
`Type` is not a `Type`.


### TypeOperators
* Pragma: `TypeOperators`
* Toggleable: False

A type may be named by an operator, a constructor's symbol such as `:+:` or a
variable's such as `+`, and declared with it written between its two parameters
or alone in brackets: `data a :+: b`, `type (+) a b`. Written between two types,
it binds as a fixity declaration in its module says of the operator written the
same. Ticked infix constructors are supported too, for `DataKinds`. In an import
or export list, a type operator named by a variable's symbol has `type` written
before it, `type (+)`, as `ExplicitNamespaces` has it.

### NamedFieldPuns
* Pragma: `NamedFieldPuns`
* Toggleable: False

Named field pun syntax is supported.

### MultilineStrings
* Pragma: `MultilineStrings`
* Toggleable: False

Multiline strings are supported.

### StandaloneDeriving
* Pragma: `StandaloneDeriving`
* Toggleable: False

Standard alone deriving is supported. A `deriving` clause on a type is taken
as the standalone declaration for each class it names, requiring that class
of each parameter of the type that it can be asked of.

# Deviations:
## Bugs
These are deviations that will are planned to get fixed at some point.

### Deriving clauses require the class of every parameter that is a type of values

A `deriving` clause is taken as `deriving instance` with the class required
of each of the type's parameters whose kind is the kind the class takes. So
nothing is required of a parameter that stands for a type that takes
arguments, or for something that is not a type of values at all. But the
class is required of a parameter that is the type of nothing the value
holds, which Haskell would not require. For that, write the
`deriving instance` out with what it requires.

## Planned Toggles
These are devitations that will eventually toggleable with a language pragma.

### Postfix Operators
Left sections are treated as function application and are not eta expanded.
This follows GHC's `PostfixOperators` extension.

## Intentional
These are deviations that are unlikely to be fixed in the the near future.

### No orphan instances
Hazy does not support orphan instances. All instance declarations must have
either the class or the data instance in the same module.

### `Int` is 63 bits
An `Int` is kept unboxed in a machine word beside a tag bit, so it has 63
bits where GHC's has 64. The report asks for at least 30. Only programs that
reach past 63 bits, or print `maxBound :: Int`, see the difference. `Int64`
has all 64.


# Copyright
Copyright © 2026 Freddy Angel Cubas "superstar64"

The compiler and other components are licensed under GPL3 only unless otherwise
specified. See `LICENSE` for more information. The standard library, and the
runtime(everything under the `library` and `runtime` subdirectories) are
licensed under the Boost License.

Any code generated by the compiler follows the license of the input source code.
This includes snippets inserted by compiler.