#!/bin/bash
set -e
docker run -v /home/rich/Documents/Projects/veg/mortality_bci:/src -it traitecoevo/mortality_bci $*
