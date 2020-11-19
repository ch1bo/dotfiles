{ config, ... }:
{
  home.file.".ghc/ghci.conf".source = ./ghci.conf;

  home.sessionPath = [
    "${config.home.homeDirectory}/.local/bin" # stack install path
    "${config.home.homeDirectory}/.cabal/bin" # cabal install path
    "${config.home.homeDirectory}/.ghcup/bin" # ghcup install path
  ];
}
