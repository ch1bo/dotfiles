{
  imports = [
    ../../desktop.nix
    ../../mail/account-ncoding.nix
    ../../mail/account-iohk.nix
  ];

  home.username = "ch1bo";
  wifi = "wlp3s0";

  # Use setuid-wrapped slock
  services.screen-locker.lockCmd = "/run/wrappers/bin/slock";

  # SSH config TODO(SN): DRY with other hosts
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "eiger" = {
        hostname = "fk.ncoding.at";
        port = 2201;
        forwardAgent = true;
        localForwards = [{ # syncthing
          bind.port = 8385;
          host.address = "127.0.0.1";
          host.port = 8384;
        }];
      };
      "liskamm" = {
        hostname = "fk.ncoding.at";
        forwardAgent = true;
        extraOptions = {
          "StreamLocalBindUnlink" = "yes";
          "RemoteForward" = "/run/user/1000/gnupg/S.gpg-agent /run/user/1000/gnupg/S.gpg-agent.extra";
        };
      };
    };
  };
}
