CABAL = cabal
GHC = $(CABAL) exec -- ghc
CXX = g++
LIBS=-lHSrts_thr -lCffi
GDALFLAGS = $(shell gdal-config --cflags)
CPPFLAGS  = $(GDALFLAGS)
CFLAGS     = $(CPPFLAGS) -fPIC
GHCFLAGS  = $(CFLAGS) -Wall -package ghc -optc-fPIC -O2 -optc-std=c++11
#GHCFLAGS += -prof -fprof-auto

all: gdal_HS.so

gdal_HS.so: GDALPlugin.hs gdal_HS.cpp
gdal_HS.cpp: GDALPlugin_stub.h
GDALPlugin.hs: cabal.sandbox.config

cabal.sandbox.config:
	$(CABAL) sandbox init
	$(CABAL) install

%.so:
	$(GHC) --make -shared $(GHCFLAGS) $^ -o $@ $(LIBS)


%_stub.h: %.hs
	$(GHC) -c $(GHCFLAGS) -O0 $< -o /dev/null

clean:
	rm -rf *.o *.hi *.so *_o_split *_stub.h .cabal-sandbox cabal.sandbox.config
