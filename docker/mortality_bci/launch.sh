#!/bin/bash
set -e
$(boot2docker shellinit 2> /dev/null)
docker run -v /Users/rich/Documents/Projects/veg/mortality_bci:/src -it richfitz/mortality_bci $*
