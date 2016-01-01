#!/bin/bash
#-----------------------------------------------------------------------------
# File:          publish.sh
# Creation Date: 2015-06-11T14:04:41+0300
# Author:        Samuli Thomasson <samuli.thomasson@paivola.fi>
# License:       See LICENSE file
#-----------------------------------------------------------------------------

DEST_BIN=hajong:/opt/hajong/bin/hajong-server.new
DEST_KETER=hajong:/opt/keter/incoming
set -e
cd $(dirname $0)

echo "Creating keter bundle..."
cd hajong-site
stack exec yesod -- keter --nobuild
mv hajong-site.keter ../dist/
cd ..

echo "Uploading keter bundle..."
scp -C ./dist/hajong-site.keter $DEST_KETER

echo "Uploading worker binary..."
scp -C ./dist/bin/hajong-server $DEST_BIN

