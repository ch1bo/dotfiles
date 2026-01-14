{
  imports = [
    ../../desktop.nix
    ../../mail/account-ncoding.nix
    ../../mail/account-iohk.nix
    ../../home-modules/ssh
  ];

  wifi = "wlp3s0";

  # TODO: man pages are broken?
  manual.manpages.enable = false;

  # Use setuid-wrapped slock
  services.screen-locker.lockCmd = "/run/wrappers/bin/slock";
}
