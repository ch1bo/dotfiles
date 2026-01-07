{
  imports = [
    ../../desktop.nix
    ../../mail/account-ncoding.nix
    ../../mail/account-iohk.nix
    ../../home-modules/ssh
  ];

  # TODO: man pages are broken?
  manual.manpages.enable = false;

  home.username = "ch1bo";
  wifi = "wlp3s0";

  # Use setuid-wrapped slock
  services.screen-locker.lockCmd = "/run/wrappers/bin/slock";
}
