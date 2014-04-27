a:
	cabal build
	cat data/1m | time ./dist/build/homework2-hs/homework2-hs 0.001 0.01 10 +RTS -K2000m -H500m -RTS
c:
	cabal build
	cat data/c | time ./dist/build/homework2-hs/homework2-hs 0.5 0.5 10 +RTS -K2000m -H500m -RTS



raw:
	cabal build
	cat /media/banacorn/8258dc47-a771-439e-9a7c-04c04ddf9ccf/d/all.txt |\
	 time ./dist/build/homework2-hs/homework2-hs 0.5 0.5 10 +RTS -K2000m -H500m -RTS > output+

all:
	cabal build
	cat /media/banacorn/8258dc47-a771-439e-9a7c-04c04ddf9ccf/d/all.txt |\
	 time ./dist/build/homework2-hs/homework2-hs 0.5 0.5 10 +RTS -K2000m -H500m -RTS > output+

part:
	cabal build
	cat /media/banacorn/8258dc47-a771-439e-9a7c-04c04ddf9ccf/d/all.txt | head -n1100k |\
	time ./dist/build/homework2-hs/homework2-hs 0.5 0.5 10 +RTS -K2000m -H500m  -sstderr -p -s -xt -hy -RTS > output+


	hp2ps -c homework2-hs.hp
	gv homework2-hs.ps 