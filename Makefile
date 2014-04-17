a:
	cabal build
	cat data/1m | time ./dist/build/homework2-hs/homework2-hs 0.001 0.01 10 +RTS -K2000m -H500m -RTS
c:
	cabal build
	cat data/c | time ./dist/build/homework2-hs/homework2-hs 0.5 0.5 10 +RTS -K2000m -H500m -RTS