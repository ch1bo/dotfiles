{
  imports = [
    ../../desktop.nix
    ../../mail/account-ncoding.nix
    ../../mail/account-iohk.nix
  ];

  # TODO: man pages are broken?
  manual.manpages.enable = false;

  home.username = "ch1bo";
  # TODO(SN): make "wifi" optional
  wifi = "wlp0s0000";

  # Use setuid-wrapped slock
  services.screen-locker.lockCmd = "/run/wrappers/bin/slock";

  # SSH config TODO(SN): re-use on other hosts?
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "weisshorn" = {
        hostname = "5.34.251.181"; # TODO: Update DNS
        port = 2222;
        forwardAgent = true;
        extraOptions = {
          "StreamLocalBindUnlink" = "yes";
          "RemoteForward" = "/run/user/1000/gnupg/S.gpg-agent /run/user/1000/gnupg/S.gpg-agent.extra";
        };
      };
      "liskamm" = {
        forwardAgent = true;
        extraOptions = {
          "StreamLocalBindUnlink" = "yes";
          "RemoteForward" = "/run/user/1000/gnupg/S.gpg-agent /run/user/1000/gnupg/S.gpg-agent.extra";
        };
      };
      "remarkable" = {
        user = "root";
        extraOptions = {
          "PubkeyAcceptedKeyTypes" = "+ssh-rsa";
          "HostKeyAlgorithms" = "+ssh-rsa";
        };
      };
      "ambicam" = {
        user = "pi";
        forwardAgent = true;
        extraOptions = {
          "PubkeyAcceptedAlgorithms" = "+ssh-rsa";
        };
      };
    };
  };
}
