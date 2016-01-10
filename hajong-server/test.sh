#!/bin/bash
#-----------------------------------------------------------------------------
# File:
# Creation Date:
# Last Modified:
# Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
#-----------------------------------------------------------------------------

cabal test --show-details=streaming --test-option="$@"
