# TODO: use xmonad from home-manager options!
{ pkgs ? import <nixpkgs> { }
}:

with pkgs;
with lib;
let
  ghc = haskellPackages.ghcWithPackages (ps: [
    ps.xmonad
    ps.xmonad-contrib
    ps.process
  ]);
  ghcVersion = concatStrings (filter (x: x != ".") (stringToCharacters ghc.version));
  hls = haskell-language-server.override
    {
      supportedGhcVersions = [ ghcVersion ];
    };
in
mkShell rec {
  buildInputs = [
    ghc
    hls
    cabal-install
    # stylish-haskell
    haskellPackages.brittany
  ];
}
