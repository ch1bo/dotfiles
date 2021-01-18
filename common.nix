{ config, lib, pkgs, ... }:
{
  imports = [
    ./connectiq
    ./emacs
    ./git
    ./haskell
    ./mail
    ./nix
    ./shell
    ./theme
    ./urxvt
    ./vim
    ./xsession
  ];

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
    home.homeDirectory = "/home/${config.home.username}";
    dotfiles = "${config.home.homeDirectory}/.dotfiles";

    home.sessionVariables = {
      EDITOR = "emacs";
    };

    home.sessionPath = [
      "${config.dotfiles}/bin"
      "${config.home.homeDirectory}/.local/bin"
    ];

    systemd.user.startServices = true;

    # Random stuff

    # Manage SSH and GPG agents
    programs.keychain = {
      enable = true;
      keys = [ ]; # Added by hand or via keepassxc
    };

    # TODO: https://github.com/NixOS/nixpkgs/issues/60012
    services.nextcloud-client.enable = true;

    home.packages = [
      pkgs.bat # cat clone with wings
      pkgs.chromium # a browser
      pkgs.docker-compose # docker projects
      pkgs.firefox # the browser
      pkgs.gnome3.eog # image viewer
      pkgs.gnome3.evince # pdf viewer
      # pkgs.gnome3.nautilus -> see README.md#Dependencies
      pkgs.jq # json processing in scripts / terminal
      pkgs.pandoc # convert everything
      pkgs.scrot # screenshots
      pkgs.keepassxc # secret stuff
      pkgs.openssl # key handling
    ];
  };
}
