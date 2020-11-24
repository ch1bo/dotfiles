{ config, ... }:
# TODO: use a flake.nix inputs instead
let
  unstable = import <unstable> { };
in
{
  home.file.".ghc/ghci.conf".source = ./ghci.conf;

  home.sessionPath = [
    "${config.home.homeDirectory}/.local/bin" # stack install path
    "${config.home.homeDirectory}/.cabal/bin" # cabal install path
  ];

  home.packages = [
    unstable.haskell-language-server
  ];
}
