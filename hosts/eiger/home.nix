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
}
