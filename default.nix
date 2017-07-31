{
  gdal-plugin-hs-dso =
    { stdenv
    , ghcWithPackages
    , cudatoolkit
    , gdal
    , llvm
    , extraPackages? ps: []
    , usePtx? false
    }:
    let ghc = ghcWithPackages
          (pkgs: [
            pkgs.gdal-plugin-hs
            pkgs.bindings-gdal
            ] ++ extraPackages pkgs);

        testScript = ''
          export GDAL_DRIVER_PATH="$(pwd)"
          echo "Testing that the driver is can be loaded..."
          gdalinfo --formats | grep -q Haskell

          export NIX_GHC="${ghc}/bin/ghc"
          export NIX_GHCPKG="${ghc}/bin/ghc-pkg"
          export NIX_GHC_DOCDIR="${ghc}/share/doc/ghc/html"
          export NIX_GHC_LIBDIR="${ghc}/lib/ghc-${ghc.version}"

          cp ${testDataset} TestDataset.hs
          export GDAL_PLUGIN_HS_UNSAFE=""
          test_the_plugin () {
            local output="$(gdalinfo -stats HS:TestDataset 2>&1)"
            echo $output
            echo $output | grep -q "Size is 2550, 1270"
            echo $output | grep -q "Block=128x256"
            echo $output | grep -q "Type=Float32"
            echo $output | grep -q "WGS_1984"
            echo $output | grep -q "NoData Value=5"
            echo $output \
              | grep -q "Minimum=42.000, Maximum=42.000, Mean=42.000, StdDev=0.000"
            echo $output \
              | grep -q "In a pattern binding: Patterns not matched: (Left _)"
          }
          echo "Testing interpreted plugins..."
          test_the_plugin
          export GDAL_PLUGIN_HS_COMPILED=""
          echo "Testing compiled plugins..."
          test_the_plugin
          '';

        testDataset = builtins.toFile "TestDataset.hs" ''
          module TestDataset where
          import GDAL
          import OSR ( srsFromEPSG )
          import GDAL.Plugin
          import GDAL.Internal.HSDataset
          import qualified Data.Vector.Storable as St

          dataset _ = return HSDataset
            { rasterSize   = 2550 :+: 1270
            , bands        = rasterBands
            , srs          = Just epsg4326
            , geotransform = Geotransform 0 0.001 0 0 0 0.001
            }
            where
              Right epsg4326 = srsFromEPSG 4326
              -- We need to type the answer so the correct band GDALDatatype can
              -- be inferred
              answer :: Float
              answer = 42
              rasterBands = [
                HSRasterBand
                  { blockSize = 128 :+: 256
                  , nodata = Just 5
                  , readBlock = const (return (St.replicate (128*256) answer))
                  }
                ]
          '';
    in stdenv.mkDerivation rec {
      version = "1.0";
      shortname = "gdal-plugin-hs-dso";
      name = "${shortname}-${version}";

      src = ./dso;

      buildInputs = [ gdal ghc ] ++ (if usePtx then [cudatoolkit] else []);

      propagatedBuildInputs = [ gdal ghc ] ++ (if usePtx then [cudatoolkit] else []);

      patchPhase = "make clean";

      installPhase = ''
        mkdir -p $out/lib
        install -m 0755 gdal_HS.so $out/lib/
        '';

      doCheck = true;

      checkPhase = testScript;

      shellHook = ''
        export GDAL_DRIVER_PATH="$(pwd):$GDAL_DRIVER_PATH"

        # Para que el compilador encuentre los paquetes y funcione ghc-mod
        export NIX_GHC="${ghc}/bin/ghc"
        export NIX_GHCPKG="${ghc}/bin/ghc-pkg"
        export NIX_GHC_DOCDIR="${ghc}/share/doc/ghc/html"
        export NIX_GHC_LIBDIR="${ghc}/lib/ghc-${ghc.version}"
        '';

      meta = {
        description = "GDAL which embeds GHC";
        homepage = https://github.com/meteogrid/gdal-plugin-hs;
        license = stdenv.lib.licenses.bsd3;
      };
    };
  gdal-plugin-hs = import ./cabal.nix;
}
