#!/bin/bash
set -e
$(boot2docker shellinit 2> /dev/null)
docker run -v /Users/dfalster/Dropbox/_research/SIEF/mortality_bci:/src -it traitecoevo/mortality_bci $*
