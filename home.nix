{ config, pkgs, ... }:
{
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

  programs.home-manager.enable = true;

  targets.genericLinux = {
    enable = true;
    # Use host's data dir as fallback, e.g. when the nixpkgs' gsettings schemas
    # are incompatible / older
    extraXdgDataDirs = [ "/usr/share" ];
  };

  imports = [
    ./git
    ./nix
    ./shell
    ./urxvt
    ./xsession
    ./gnome
    ./emacs
    ./vim
    ./haskell
    ./connectiq
  ];

  # Random packages
  home.packages = [
    pkgs.docker-compose
  ];
}
