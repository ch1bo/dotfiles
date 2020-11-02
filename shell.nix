{ pkgs ? import <nixpkgs> { } }:
let
  home-manager = (import
    (builtins.fetchGit {
      url = "https://github.com/nix-community/home-manager.git";
      ref = "release-20.09";
      rev = "";
    })
    { inherit pkgs; });
in
pkgs.mkShell {
  buildInputs = [ home-manager.home-manager ];
  HOME_MANAGER_CONFIG = ./home.nix;
}
