.PHONY: clean coverage test

clean:
	cabal clean


coverage: clean
	cabal configure --enable-coverage && cabal build && cabal test

test:
	cabal build && cabal test
