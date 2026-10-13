# The build has two stages: cabal builds the compiler, and then the compiler
# builds the runtime and base packages. tools/build.sh does both into DIST.
#
#   make                 build the compiler and its packages
#   make run             start an interactive session
#   make run ARGS="..."  run the compiler with some arguments instead
#   make test            run the tests
#   make bootstrap       have Hazy compile itself twice over, and compare
#   make bootstrap WHAT=native   the same with the compiler as an executable
#   make clean           remove what was built, but not cabal's own cache

DIST ?= .dist
HAZY := $(DIST)/bin/hazy

# Written when a build finishes, so that a build that failed is tried again.
BUILT := $(DIST)/.built

SOURCES := $(shell find compiler diagnostics viewer runtime library -type f) \
	hazy.cabal cabal.project tools/build.sh

# What `make run` gives the compiler when it is told nothing else.
ARGS ?= --repl

# Which of its forms `make bootstrap` has the compiler make itself in.
WHAT ?= javascript

.PHONY: all run test bootstrap clean

all: $(BUILT)

$(BUILT): $(SOURCES)
	env bash tools/build.sh $(DIST)
	@touch $@

run: all
	@$(HAZY) $(ARGS)

test: all
	env bash tools/test.sh $(HAZY)

bootstrap: all
	env bash tools/bootstrap.sh $(HAZY) $(WHAT)

clean:
	rm -rf $(DIST) .test .bootstrap
