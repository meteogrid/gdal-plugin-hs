{ pkgs   ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, gdal   ? pkgs.gdal
, haskellPackages ? pkgs.haskell.compiler.ghc801
}:

let
    ghc = haskellPackages.ghcWithPackages (p: with p; [
      gdal-plugin-hs
      ]);
in
stdenv.mkDerivation rec {
  version = "1.0";
  shortname = "gdal-plugin-hs-dso";
  name = "${shortname}-${version}";

  src = ./.;

  buildInputs = [
    gdal
    ghc
  ];

  installPhase = ''
    mkdir -p $out/lib
    install -m 0755 dso/gdal_HS.so $out/lib/
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
}
