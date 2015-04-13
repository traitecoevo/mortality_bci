#!/bin/sh
set -e
docker run -v `pwd`:/host -it mortality_bci_worker $*
