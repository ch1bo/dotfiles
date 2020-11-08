{ config, pkgs, ... }:
{
  programs.home-manager.enable = true;
  home.stateVersion = "20.09";
  home.username = "ch1bo";
  home.homeDirectory = "/home/ch1bo";

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  home.sessionPath = [
    "$HOME/.dotfiles/bin"
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
    ./emacs
    ./vim
    ./haskell
    ./connectiq
  ];

  home.packages = [
    pkgs.docker-compose
  ];
}
