#!/usr/bin/env bash

OUTPUT_DIRECTORY=/output
VCARD_STUDIO_SOURCE_DIR=/src


# build vCard Studio
cd "${VCARD_STUDIO_SOURCE_DIR}/Install/deb"
./build.sh 

# move the result from build-root to the output directory
cp /tmp/build-root/vcard-studio_*_amd64.deb "${OUTPUT_DIRECTORY}"

