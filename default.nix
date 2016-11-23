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

    shellHook = ''
      export GDAL_DRIVER_PATH="$(pwd)/dso:$GDAL_DRIVER_PATH"

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
