{ pkgs, ... }:
{
  imports = [
    ./base.nix
    ./emacs
    ./gpg
    ./haskell
    ./mail
    ./theme
    ./xsession
    ./home-modules/passwords
    ./home-modules/browser/zen.nix
  ];

  config = {
    # To sync stuff with server
    services.nextcloud-client.enable = true;

    # Automount removable devices
    services.udiskie = {
      enable = true;
      tray = "never";
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
      eog # image viewer
      evince # pdf viewer
      # nautilus -> see README.md#Dependencies
      pandoc # convert everything
      spotify # unlimited music
      signal-desktop # connect with rl
      slack # comms
      remarkable-mouse # drawing tablets ftw
      restream # live stream remarkable
      gh # github utility
    ];
  };
}
