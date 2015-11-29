#!/bin/bash
#-----------------------------------------------------------------------------
# File:          publish.sh
# Creation Date: 2015-06-11T14:04:41+0300
# Author:        Samuli Thomasson <samuli.thomasson@paivola.fi>
# License:       See LICENSE file
#-----------------------------------------------------------------------------

DEST=hajong:/opt/hajong
set -e
cd $(dirname $0)
rsync -av ./dist/ $DEST
