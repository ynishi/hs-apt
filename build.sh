#!/bin/sh

set -eux

hlint src/*.hs app/*.hs
hfmt -w src/*.hs app/*.hs
stack build
stack test
stack install

stack exec -- sysup-exe --help 
