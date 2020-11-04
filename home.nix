{ config, pkgs, ... }:
let
  dotFilesDirectory = "$HOME/.dotfiles";
in
{
  programs.home-manager.enable = true;
  home.stateVersion = "20.09";
  home.username = "ch1bo";
  home.homeDirectory = "/home/ch1bo";
  home.sessionVariables = {
    DOTFILES = dotFilesDirectory;
    EDITOR = "emacs";
  };
  home.sessionPath = [
    "${dotFilesDirectory}/bin"
  ];
  imports = [
    ./git
    ./nix
    ./shell
  ];
}
