{ pkgs, config, unstable, ... }:
{
  home.file.".ghc/ghci.conf".source = ./ghci.conf;
  home.file.".stack/config.yaml".source = ./stack-config.yaml;
  xdg.configFile."brittany/config.yaml".source = ./brittany-config.yaml;


  home.sessionPath = [
    "${config.home.homeDirectory}/.local/bin" # stack install path
    "${config.home.homeDirectory}/.cabal/bin" # cabal install path
  ];

  # NOTE Formatters are not picked up from project nix-shells (why?); also use
  # latest versions
  home.packages = [
    unstable.haskellPackages.cabal-fmt
    unstable.haskellPackages.fourmolu
    unstable.haskellPackages.stylish-haskell
    unstable.haskellPackages.hp2pretty # used in bin/haskell-view-heap
  ];
}
