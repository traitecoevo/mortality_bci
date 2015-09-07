#!/bin/bash
set -e
eval '$(docker-machine env default)'
docker run -v /Users/dfalster/Dropbox/_research/SIEF/mortality_bci:/src -it traitecoevo/mortality_bci $*
