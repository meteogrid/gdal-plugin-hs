GHC = ghc
CXX = g++
LIBS=-lHSrts_thr-ghc$(shell ghc --numeric-version)  -lCffi_thr -lrt
GDALFLAGS = $(shell gdal-config --cflags)
CPPFLAGS  = $(GDALFLAGS)
CFLAGS     = $(CPPFLAGS)
GHCFLAGS  = $(CFLAGS) -Wall -O2
#GHCFLAGS += -prof -fprof-auto
GHCFLAGS += -dynamic

all: gdal_HS.so

gdal_HS.so: GDALPlugin.hs gdal_HS.cpp initRts.c
gdal_HS.cpp: GDALPlugin_stub.h

%.so:
	$(GHC) --make -shared $(GHCFLAGS) $^ -o $@ $(LIBS)


%_stub.h: %.hs
	$(GHC) -c $(GHCFLAGS) -O0 $< -o /dev/null

clean:
	rm -rf *.o *.dyn_o *.hi *.dyn_hi *.so *_o_split *_stub.h .cabal-sandbox cabal.sandbox.config
