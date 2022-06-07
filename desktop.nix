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
    # To sync stuff with server
    # TODO: https://github.com/NixOS/nixpkgs/issues/60012
    services.nextcloud-client.enable = true;

    # Automount removable devices
    services.udiskie = {
      enable = true;
      tray = "never";
    };

    # Start mic effect preset on login
    services.pulseeffects = {
      enable = true;
      package = pkgs.pulseeffects-legacy;
      preset = "Rode";
    };


    # the browser
    programs.firefox.enable = true;
    programs.firefox.profiles.ch1bo.extraConfig = ''
      // Allow file:// links
      user_pref("capability.policy.policynames", "localfilelinks");
      user_pref("capability.policy.localfilelinks.sites", "http://localhost:8080");
      user_pref("capability.policy.localfilelinks.checkloaduri.enabled", "allAccess");
    '';

    home.packages = [
      pkgs.docker-compose # docker projects
      pkgs.docker-credential-helpers # store docker login credentials in D-Bus secrets
      pkgs.brave # another browser
      pkgs.gnome.eog # image viewer
      pkgs.gnome.evince # pdf viewer
      # pkgs.gnome.nautilus -> see README.md#Dependencies
      pkgs.pandoc # convert everything
      pkgs.keepassxc # secret stuff
      pkgs.spotify # unlimited music
      pkgs.signal-desktop # connect with rl
      pkgs.slack # comms
      pkgs.skypeforlinux # old-school comms
    ];
  };
}
