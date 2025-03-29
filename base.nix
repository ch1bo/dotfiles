{ config, lib, pkgs, ... }:
{
  imports = [
    ./git
    ./nix
    ./shell
    ./terminal
    ./vim
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
      EDITOR = "vim";
    };

    home.sessionPath = [
      "${config.dotfiles}/bin"
      "${config.home.homeDirectory}/.local/bin"
    ];

    systemd.user.startServices = true;

    # Random stuff

    home.packages = [
      pkgs.bat # cat clone with wings
      pkgs.jq # json processing in scripts / terminal
      pkgs.openssl # key handling
    ];
  };
}
