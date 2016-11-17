#!/usr/bin/env bash

CFLAGS=$(gdal-config --cflags)

# generate stubs
ghc -c -fPIC -dynamic GDALPlugin.hs

# build shared lib
ghc --make -fPIC -shared -dynamic \
$CFLAGS GDALPlugin.hs gdal_HS.cpp -o gdal_HS.so -lHSrts_thr-ghc8.0.1
