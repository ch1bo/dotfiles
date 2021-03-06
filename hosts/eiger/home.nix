{
  imports = [
    ../../desktop.nix
    ../../mail/account-ncoding.nix
    ../../mail/account-iohk.nix
  ];

  home.username = "ch1bo";
  # TODO(SN): make "wifi" optional
  wifi = "wlp0s0000";

  # Use setuid-wrapped slock
  services.screen-locker.lockCmd = "/run/wrappers/bin/slock";

  # SSH config TODO(SN): re-use on other hosts?
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "liskamm" = {
        forwardAgent = true;
        extraOptions = {
          "StreamLocalBindUnlink" = "yes";
          "RemoteForward" = "/run/user/1000/gnupg/S.gpg-agent /run/user/1000/gnupg/S.gpg-agent.extra";
        };
      };
    };
  };
}
