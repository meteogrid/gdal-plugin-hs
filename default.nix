{
  gdal-plugin-hs-dso = {stdenv, ghc, gdal}: stdenv.mkDerivation rec {
    version = "1.0";
    shortname = "gdal-plugin-hs-dso";
    name = "${shortname}-${version}";

    src = ./dso;

    buildInputs = [
      gdal
      ghc
    ];


    patchPhase = "make clean";

    installPhase = ''
      mkdir -p $out/lib
      install -m 0755 gdal_HS.so $out/lib/
      '';

    doCheck = true;

    checkPhase = ''
      export GDAL_DRIVER_PATH="$(pwd)"
      gdalinfo --formats | grep Haskell
      '';

    meta = {
      description = "GDAL which embeds GHC";
      homepage = https://github.com/meteogrid/gdal-plugin-hs;
      license = stdenv.lib.licenses.bsd3;
    };
  };
  gdal-plugin-hs = import ./cabal.nix;
}
