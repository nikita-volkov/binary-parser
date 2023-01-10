all: format test

test:
	cabal test -j +RTS -A128m -n2m -N -RTS -fwarn-incomplete-patterns --builddir dist/test

docs:
	cabal haddock --enable-documentation

format:
	path=binary-parser.cabal && cabal-fmt -c $$path || cabal-fmt -i $$path
	ormolu --mode inplace -c $$(find . -name "*.hs" -path "./library/*" && find . -name "*.hs" -path "./tests/*")

clean:
	rm -rf dist
