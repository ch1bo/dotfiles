{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.nixpkgs-fmt
  ];

  # Use default paths instead of config.nixpkgs for nix-env etc.
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/overlays.nix".source = ./nixpkgs-overlays.nix;
}
