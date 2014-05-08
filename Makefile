
all:
	cabal build
	cat data/final | time ./dist/build/homework2-hs/homework2-hs 0.001 0.01 10 +RTS -K2000m -H500m -RTS

part:
	cabal build
	cat data/final | head -n1000000 |\
	time ./dist/build/homework2-hs/homework2-hs 0.0007 0.01 10 +RTS -K2000m -H500m -RTS > output

p:
	cat data/final | head -n2000000 |\
	time ./dist/build/homework2-hs/homework2-hs 0.001 0.01 10 +RTS -sstderr -p -s -xt -hy -RTS


	hp2ps -c homework2-hs.hp
	open homework2-hs.ps 

allraw:
	cabal build
	cat /media/banacorn/8258dc47-a771-439e-9a7c-04c04ddf9ccf/d/all.txt |\
	 time ./dist/build/homework2-hs/homework2-hs 0.5 0.5 10 +RTS -K2000m -H500m -RTS > output+

partraw:
	cabal build
	cat /media/banacorn/8258dc47-a771-439e-9a7c-04c04ddf9ccf/d/all.txt | head -n1100k |\
	time ./dist/build/homework2-hs/homework2-hs 0.5 0.5 10 +RTS -K2000m -H500m  -sstderr -p -s -xt -hy -RTS > output+


	hp2ps -c homework2-hs.hp
	gv homework2-hs.ps 