{ config, lib, pkgs, ... }:
{
  imports = [
    ./base.nix
    ./connectiq
    ./emacs
    ./gpg
    ./haskell
    ./mail
    ./theme
    ./xsession
  ];

  config = {
    # TODO: https://github.com/NixOS/nixpkgs/issues/60012
    services.nextcloud-client.enable = true;

    home.packages = [
      pkgs.chromium # a browser
      pkgs.docker-compose # docker projects
      pkgs.firefox # the browser
      pkgs.gnome3.eog # image viewer
      pkgs.gnome3.evince # pdf viewer
      # pkgs.gnome3.nautilus -> see README.md#Dependencies
      pkgs.pandoc # convert everything
      pkgs.scrot # screenshots
      pkgs.keepassxc # secret stuff
      pkgs.spotify # unlimited music
      pkgs.signal-desktop # connect with rl
      pkgs.skype # old-school comms
    ];
  };
}
