{ pkgs, config, ... }:
let
  # TODO use nix flakes
  unstable = import
    (builtins.fetchTarball {
      # Descriptive name to make the store path easier to identify
      name = "nixpkgs-unstable-2021-01-04";
      url = "https://github.com/nixos/nixpkgs/archive/56bb1b0f7a33e5d487dc2bf2e846794f4dcb4d01.tar.gz";
      sha256 = "1wl5yglgj3ajbf2j4dzgsxmgz7iqydfs514w73fs9a6x253wzjbs";
    })
    { };
in
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
    unstable.haskellPackages.brittany
    unstable.haskellPackages.stylish-haskell
  ];
}
