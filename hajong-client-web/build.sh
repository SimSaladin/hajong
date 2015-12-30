#!/bin/bash
#-----------------------------------------------------------------------------
# File:          hajong-client-web/build.sh
# Creation Date: 2015-11-29T14:05:11+0200
# Author:        Samuli Thomasson <samuli.thomasson@paivola.fi>
# License:       WTFPL
#-----------------------------------------------------------------------------

set -e

cd $(dirname $0)
mkdir -p dist/images
elm-make src/ViewingGame.elm src/Main.elm --output dist/elm.js
cp images/* dist/images

