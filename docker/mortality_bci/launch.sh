#!/bin/sh
set -e
docker run --rm --link redis:redis -v `pwd`:/host -it mortality_bci $*
