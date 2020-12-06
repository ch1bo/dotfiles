{ config, lib, pkgs, ... }:
{
  imports = [
    ./connectiq
    ./emacs
    ./git
    ./gnome
    ./haskell
    ./mail
    ./nix
    ./shell
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

    programs.home-manager.enable = true;
    systemd.user.startServices = true;

    # Random stuff
    services.nextcloud-client.enable = true;
    home.packages = [
      pkgs.bat # cat clone with wings
      pkgs.chromium # a browser
      pkgs.docker-compose # docker projects
      pkgs.firefox # the browser
      pkgs.jq # json processing in scripts / terminal
      pkgs.pandoc # convert everything
      pkgs.scrot # screenshots
    ];
  };
}
