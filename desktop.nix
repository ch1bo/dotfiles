{ config, lib, pkgs, unstable, ... }:
{
  imports = [
    ./base.nix
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

    # Default applications for files
    xdg.mimeApps.defaultApplications = {
      "application/pdf" = "firefox.desktop";
    };

    # the browser
    programs.firefox.enable = true;
    programs.firefox.profiles.ch1bo.extraConfig = ''
      // Allow file:// links
      user_pref("capability.policy.policynames", "localfilelinks");
      user_pref("capability.policy.localfilelinks.sites", "http://localhost:8080");
      user_pref("capability.policy.localfilelinks.checkloaduri.enabled", "allAccess");
    '';

    home.packages = with pkgs; [
      docker-compose # docker projects
      docker-credential-helpers # store docker login credentials in D-Bus secrets
      brave # another browser
      gnome.eog # image viewer
      gnome.evince # pdf viewer
      # gnome.nautilus -> see README.md#Dependencies
      pandoc # convert everything
      keepassxc # secret stuff
      spotify # unlimited music
      unstable.signal-desktop # connect with rl
      slack # comms
      skypeforlinux # old-school comms
      restream # live stream remarkable
      gh # github utility
    ];
  };
}
