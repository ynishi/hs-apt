#/bin/sh

set -eux

sudo stack exec --allow-different-user -- $(basename $(pwd))-exe -t apt
stack exec -- $(basename $(pwd))-exe -t stack
