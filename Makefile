.PHONY: all bench build clean configure haddock hpc install repl test

all: install configure build haddock test hpc bench

bench:
	cabal bench

build:
	cabal build

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-benchmarks --enable-tests --enable-coverage -v2

haddock:
	cabal haddock --hyperlink-source
	open dist/doc/html/gitson/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	open tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-benchmarks --enable-tests --only-dependencies --reorder-goals

repl:
	cabal repl lib:gitson --ghc-options="-fno-hpc"

test:
	cabal test --show-details=always --test-option=--color
