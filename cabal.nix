{ mkDerivation, accelerate, accelerate-io, accelerate-llvm-native
, accelerate-llvm-ptx, base, bindings-gdal, bytestring
, data-default, deepseq, directory, ghc, ghc-paths, http-types
, stdenv, temporary, text, vector, usePtx?false
}:
mkDerivation ({
  pname = "gdal-plugin-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    accelerate accelerate-io accelerate-llvm-native
    base bindings-gdal bytestring data-default deepseq directory ghc
    ghc-paths http-types temporary text vector
  ] ++ (if usePtx then [accelerate-llvm-ptx] else []);
  license = stdenv.lib.licenses.bsd3;
} // (if usePtx then { configureFlags = ["-fptx"];} else {}))
