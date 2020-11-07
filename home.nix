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
    "$HOME/.local/bin"
  ];

  # TODO try this out
  targets.genericLinux.enable = true;

  imports = [
    ./git
    ./nix
    ./shell
    ./urxvt
    ./x
    ./haskell
    ./connectiq
  ];

  home.packages = [
    pkgs.docker-compose
  ];
}
