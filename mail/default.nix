{ config, lib, pkgs, ... }:
{
  # TODO use options / mkIf instead? also, how to deduplicate with emacs config?
  # Include any of the ./account-xxx.nix modules

  home.packages = [
    pkgs.gnome3.libsecret # secret-tool
  ];

  accounts.email.maildirBasePath = "mail";

  programs.mu.enable = true;
  programs.msmtp.enable = true;
  programs.offlineimap.enable = true;

  systemd.user.services.offlineimap = {
    Unit.Description = "offlineimap service";
    Service = {
      Type = "oneshot";
      Environment = "PATH=${lib.makeBinPath [ pkgs.mu pkgs.gnome3.libsecret ]}";
      ExecStart = "${pkgs.offlineimap}/bin/offlineimap -u syslog -o -1";
    };
  };
  systemd.user.timers.offlineimap = {
    Unit.Description = "offlineimap timer";
    Timer = {
      Unit = "offlineimap.service";
      OnCalendar = "*:0/5"; # every 5 minutes
    };
    Install = { WantedBy = [ "timers.target" ]; };
  };
}
