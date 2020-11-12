{ config, lib, pkgs, ... }:
{
  options = {
    dotfiles = lib.mkOption {
      type = lib.types.path;
      apply = toString;
      example = "${config.home.homeDirectory}/.dotfiles";
      description = "Location of the dotfiles working copy";
    };
  };

  config = {
    home.stateVersion = "20.09";
    home.username = "ch1bo";
    home.homeDirectory = "/home/ch1bo";
    dotfiles = "${config.home.homeDirectory}/.dotfiles";

    home.sessionVariables = {
      EDITOR = "emacs";
    };

    home.sessionPath = [
      "${config.dotfiles}/bin"
      "${config.home.homeDirectory}/.local/bin"
    ];

    programs.home-manager.enable = true;

    targets.genericLinux = {
      enable = true;
      # Use host's data dir as fallback, e.g. when the nixpkgs' gsettings schemas
      # are incompatible / older
      extraXdgDataDirs = [ "/usr/share" ];
    };

    # Random packages
    home.packages = [
      pkgs.docker-compose
    ];
  };

  imports = [
    ./connectiq
    ./emacs
    ./git
    ./gnome
    ./haskell
    ./mail
    ./mail/account-ncoding.nix
    ./nix
    ./shell
    ./urxvt
    ./vim
    ./xsession
  ];
}
