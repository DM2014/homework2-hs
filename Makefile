a:
	cabal build
	time ./dist/build/homework2-hs/homework2-hs 0.001 0.5 10 +RTS -K2000m -H500m -RTS