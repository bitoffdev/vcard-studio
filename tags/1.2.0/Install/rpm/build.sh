#!/bin/bash

APP_NAME=vcard-studio
tar -zcvf ${APP_NAME}.tar.gz -c ../.. .
mkdir -p ~/rpmbuild/SOURCES
cp ${APP_NAME}.tar.gz ~/rpmbuild/SOURCES
rpmbuild -v -ba ${APP_NAME}.spec
