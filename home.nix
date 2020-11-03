{ config, ... }:

{
  programs.home-manager.enable = true;
  home.username = "ch1bo";
  home.homeDirectory = "/home/ch1bo";
  home.stateVersion = "20.09";

  home.packages = [ ];

  # Since we do not install home-manager, you need to let home-manager
  # manage your shell, otherwise it will not be able to add its hooks
  # to your profile.
  # programs.bash = {
  #   enable = true;
  # };

  # nix-env looks for this file
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs/config.nix;
  xdg.configFile."nixpkgs/overlays.nix".source = ./nixpkgs/overlays.nix;
}
