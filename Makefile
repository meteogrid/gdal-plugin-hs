GHC = ghc
C2HS = c2hs
CXX = g++
LIBS=-lHSrts_thr -lCffi
GDALFLAGS = $(shell gdal-config --cflags)
CPPFLAGS  = $(GDALFLAGS)
CFLAGS     = $(CPPFLAGS) -fPIC
GHCFLAGS  = $(CFLAGS) -Wall -optc-fPIC -O2
#GHCFLAGS += -prof -fprof-auto

all: plugin

plugin: gdal_HS.so

gdal_HS.so: gdal_HS.cpp GDALPlugin.hs

gdal_HS.cpp: GDALPlugin_stub.h gdal_HS.h
GDALPlugin.hs: gdal_HS.h

%.so:
	$(GHC) --make -shared $(GHCFLAGS) $^ -o $@ $(LIBS)

%.hs: %.chs
	$(C2HS) -C $(CPPFLAGS) $< -o $@

%_stub.h: %.hs
	$(GHC) -c $(GHCFLAGS) $< -o /dev/null

clean:
	rm -rf *.o *.hi *.chi GDALPlugin.hs *.chs.h *.so *_o_split *_stub.h
