{ pkgs   ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, gdal   ? pkgs.gdal
, haskellPackages ? pkgs.haskell.compiler.ghc801
}:

let
    ghc = haskellPackages.ghcWithPackages (p: with p; [
      bindings-gdal
      c2hs
      ]);
in
stdenv.mkDerivation rec {
  version = "1.0";
  shortname = "gdal-plugin-hs";
  name = "${shortname}-${version}";

  src = ./.;

  buildInputs = [
    gdal
    ghc
  ];

  shellHook = ''
    runHook setVariables
    runHook preShellHook
    export NIX_GHC="${ghc.out}/bin/ghc"
    export NIX_GHCPKG="${ghc.out}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${ghc.out}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR="${ghc.out}/lib/${ghc.name}"
    '';

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
}
