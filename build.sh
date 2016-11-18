#!/usr/bin/env bash

set -e

CFLAGS=$(gdal-config --cflags)

# generate hs file
c2hs -C "$CFLAGS" GDALPlugin.chs -o GDALPlugin.hs

# generate stubs
ghc -c -fPIC -optc-fPIC GDALPlugin.hs

# build shared lib
ghc --make -fPIC -shared -optc-fPIC \
  $CFLAGS GDALPlugin.hs gdal_HS.cpp \
  -o gdal_HS.so -lHSrts_thr -lCffi "$@"
