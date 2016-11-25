{ mkDerivation, base, bindings-gdal, bytestring, data-default
, deepseq, directory, ghc, ghc-paths, http-types, stdenv, temporary
, text, vector
}:
mkDerivation {
  pname = "gdal-plugin-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bindings-gdal bytestring data-default deepseq directory ghc
    ghc-paths http-types temporary text vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
