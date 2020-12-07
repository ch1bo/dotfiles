{ pkgs, config, ... }:
{
  home.file.".ghc/ghci.conf".source = ./ghci.conf;
  xdg.configFile."brittany/config.yaml".source = ./brittany-config.yaml;

  home.sessionPath = [
    "${config.home.homeDirectory}/.local/bin" # stack install path
    "${config.home.homeDirectory}/.cabal/bin" # cabal install path
  ];

  # NOTE Formatters are not picked up from project nix-shells (yet)
  home.packages = [
    pkgs.haskellPackages.brittany
    pkgs.haskellPackages.stylish-haskell
  ];
}
