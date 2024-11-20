# Bitwarden self-hosted variant
#
# Container config ported from
# https://github.com/dani-garcia/vaultwarden
#
# Including fail2ban and off-site backups using borgbase.
{ config, pkgs, lib, ... }:
let
  # Check release notes
  # https://github.com/dani-garcia/vaultwarden/releases
  version = "1.32.5";
  port = 3876; # not exposed
  domain = "passwords.ncoding.at";
in
{
  services.nginx.virtualHosts.${domain} = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
    };
  };

  virtualisation.oci-containers.containers.vaultwarden = {
    image = "ghcr.io/dani-garcia/vaultwarden:${version}";
    environment = {
      DOMAIN = "https://${domain}";
      SIGNUPS_ALLOWED = "false";
      SHOW_PASSWORD_HINT = "false";
      ADMIN_TOKEN = "$argon2id$v=19$m=19456,t=2,p=1$C0Iqwm7K4IO6280xA3Ki06MILPS7i5e5qmcI3v0nXlA$gfrExado4UTb1dyjIBpyg/3GWhrmaeQvIK95xjP4msc";
    };
    ports = [ "${toString port}:80" ];
    volumes = [
      "/data/vaultwarden:/data"
    ];
  };

  # Backup to borgbase
  services.borgbackup.jobs.vaultwarden = {
    paths = [ "/data/vaultwarden" ];
    doInit = true;
    repo = "j3er42wm@j3er42wm.repo.borgbase.com:repo";
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat /root/keys/borg/vaultwarden.pass";
    };
    environment.BORG_RSH = "ssh -i /root/keys/borg/id_ed25519";
    compression = "auto,lzma";
    startAt = "daily";
    prune.keep = {
      daily = 7; # Keep 7 daily archives
      weekly = 4; # Keep 4 weekly archives
      monthly = 12; # Keep 12 monthly archives
    };
  };

  # Fail2ban blocking of failed login attempts
  # https://github.com/dani-garcia/vaultwarden/wiki/Fail2Ban-Setup
  services.fail2ban.enable = true;
  services.fail2ban.jails.vaultwarden.settings = {
    enabled = true;
    filter = "vaultwarden";
    backend = "systemd";
    findtime = "1d";
    bantime = "1d";
    maxretry = 3;
  };
  environment.etc."fail2ban/filter.d/vaultwarden.local".text = ''
    [Definition]
    failregex = ^.*?Username or password is incorrect\. Try again\. IP: <ADDR>\. Username:.*$
    journalmatch = CONTAINER_NAME=vaultwarden
  '';
}
