{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;
  home.username = "ch1bo";
  home.homeDirectory = "/home/ch1bo";
  home.stateVersion = "20.09";
  home.packages = [ ];
}
