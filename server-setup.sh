#!/bin/bash
#-----------------------------------------------------------------------------
# File:          server-setup.sh
# Creation Date: 2015-06-11T14:34:20+0300
# Author:        Samuli Thomasson <samuli.thomasson@paivola.fi>
# License:       See the LICENSE file
#-----------------------------------------------------------------------------

sudo mkdir /opt/hajong
cd /opt/hajong
sudo chown ec2-user:ec2-user .

#

mkdir bin

echo "- copy libgmp.so.10 to /usr/lib64"
