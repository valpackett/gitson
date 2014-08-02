.PHONY: all bench build clean configure haddock hpc install repl run test

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
	cabal configure --enable-benchmarks --enable-tests

haddock:
	cabal haddock --hyperlink-source
	# dist/doc/html/gitson/index.html

install:
	cabal sandbox init
	cabal install --enable-benchmarks --enable-tests --only-dependencies --reorder-goals

repl:
	cabal repl lib:gitson

test:
	cabal test tests
