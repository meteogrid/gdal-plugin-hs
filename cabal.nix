{ mkDerivation, base, bindings-gdal, bytestring, data-default
, deepseq, ghc, ghc-paths, http-types, directory, sigym4-dimension
, spatial-reference, stdenv, temporary, text, vector
}:
mkDerivation {
  pname = "gdal-plugin-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bindings-gdal bytestring data-default deepseq ghc ghc-paths
    http-types directory sigym4-dimension spatial-reference temporary text vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
