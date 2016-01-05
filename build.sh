#!/bin/bash
#-----------------------------------------------------------------------------
# File:          build.sh
# Creation Date: 2015-06-11T14:04:41+0300
# Author:        Samuli Thomasson <samuli.thomasson@paivola.fi>
# License:       See LICENSE file
#-----------------------------------------------------------------------------

set -e

cd $(dirname $0)

hajong-client-web/build.sh

stack build $@

BIN_LOCATION=$(stack path --local-install-root | cut -d\  -f 2)
rsync -a $BIN_LOCATION/bin/ dist/bin
