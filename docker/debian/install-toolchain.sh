#!/usr/bin/env bash

set -ex

# Install dependencies from apt:
# - curl is required to download debs
# - binutils is required by fpc-laz
# - libgtk2.0-dev is required by lazarus-project
apt-get update
apt-get install -y binutils curl libgtk2.0-dev

#
# fetch lazarus packages
#
cd /tmp
curl -LO https://downloads.sourceforge.net/project/lazarus/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.2.0/fpc-laz_3.2.2-210709_amd64.deb
curl -LO https://downloads.sourceforge.net/project/lazarus/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.2.0/lazarus-project_2.2.0-0_amd64.deb
curl -LO https://downloads.sourceforge.net/project/lazarus/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.2.0/fpc-src_3.2.2-210709_amd64.deb

#
# install lazarus packages
#
dpkg -i fpc-laz_3.2.2-210709_amd64.deb 
dpkg -i fpc-src_3.2.2-210709_amd64.deb 
dpkg -i lazarus-project_2.2.0-0_amd64.deb 
