# TODO: use xmonad from home-manager options!
{ pkgs ? import <unstable> { }
, compilerVersion ? "8102"
, compiler ? "ghc${compilerVersion}"
}:

with pkgs;
let
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: [
    ps.xmonad
    ps.xmonad-contrib
    ps.process
  ]);
  hls = pkgs.haskell-language-server.override
    { supportedGhcVersions = [ compilerVersion ]; };
in
pkgs.mkShell rec {
  buildInputs = [
    ghc
    hls
    # pkgs.cabal-install
    # pkgs.stylish-haskell
    pkgs.haskell.packages.${compiler}.brittany
  ];
}
