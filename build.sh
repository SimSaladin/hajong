#!/bin/bash
#-----------------------------------------------------------------------------
# File:          build.sh
# Creation Date: 2015-06-11T14:04:41+0300
# Author:        Samuli Thomasson <samuli.thomasson@paivola.fi>
# License:       See LICENSE file
#-----------------------------------------------------------------------------

DEST=hajong:/opt/hajong

set -e

cd $(dirname $0)

[[ "$1" =~ "b" ]] && cabal install ./hajong-site ./hajong-server

rsync -av ./.cabal-sandbox/bin/hajong-{server,site} $DEST/bin
rsync -avL ./hajong-site/static/ $DEST/static
rsync -av ./hajong-site/config/{settings.yml,sqlite.yml,robots.txt,favicon.ico} $DEST/config/
