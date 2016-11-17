{ pkgs   ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, gdal   ? pkgs.gdal
, ghc    ? pkgs.haskell.compiler.ghc801
}:

stdenv.mkDerivation rec {
  version = "1.0";
  shortname = "gdal_gribapi";
  name = "${shortname}-${version}";

  src = ./.;

  buildInputs = [ gdal ghc ];

  buildPhase = ''
    ./build.sh
    '';

  installPhase = ''
    mkdir -p $out/lib
    install -m 0755 gdal_HS.so $out/lib/
    '';

  doCheck = false;

  checkPhase = ''
    export GDAL_DRIVER_PATH="$(pwd)"
    gdalinfo --formats | grep Haskell
    '';

  meta = {
    description = "GDAL which embeds GHC";
    homepage = https://github.com/meteogrid/gdal-plugin-hs;
    license = stdenv.lib.licenses.bsd3;
  };
}
