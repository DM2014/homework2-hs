homework 2
============

# How to run the code

## Prerequisite

* haskell-platform
* git (optional)

## Install

You may skip the `git` part and just download it.

```shell
git clone git@github.com:DM2014/homework2-hs.git
cd homework2-hs
cabal sandbox init
cabal update
cabal install --only-dependencies
```

## build

```shell
cabal build
```
## Run

```shell
cat <dataset> | ./dist/build/homework2-hs/homework2-hs <support> <confidence> <k> +RTS -K1000m -H500m -RTS
```
