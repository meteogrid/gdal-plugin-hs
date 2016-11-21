{ mkDerivation, base, bindings-gdal, bytestring, data-default
, deepseq, ghc, ghc-paths, http-types, stdenv, temporary, text
, vector, gdal
}:
mkDerivation {
  pname = "gdal-plugin-hs";
  version = "0.1.0.0";
  src = ./.;
  librarySystemDepends = [ gdal ];
  libraryHaskellDepends = [
    base bindings-gdal bytestring data-default deepseq ghc ghc-paths
    http-types temporary text vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
