# Home assistant as container
#
# https://www.home-assistant.io/installation/linux#install-home-assistant-container
# https://nixos.wiki/wiki/Home_Assistant#OCI_container
#
# Including fail2ban and off-site backups using borgbase.
{ config, pkgs, lib, inputs, system, ... }:
let
  # Check release notes
  # https://github.com/home-assistant/core/releases
  version = "2026.2.3";
  port = 8123; # not exposed
in
{
  services.nginx.virtualHosts."home.ncoding.at" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
    };
  };

  virtualisation.oci-containers.containers.home-assistant = {
    image = "ghcr.io/home-assistant/home-assistant:${version}";
    volumes = [ "/data/home-assistant:/config" ];
    environment.TZ = "Europe/Vienna";
    extraOptions = [
      "--network=host"
      "--device=/dev/ttyUSB0:/dev/ttyUSB0"
    ];
  };

  # Backup to borgbase
  services.borgbackup.jobs.home-assistant = {
    paths = [ "/data/home-assistant" ];
    doInit = true;
    repo = "n8j084i7@n8j084i7.repo.borgbase.com:repo";
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat /root/keys/borg/home-assistant.pass";
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
  services.fail2ban.enable = true;
  services.fail2ban.jails.home-assistant.settings = {
    enabled = true;
    filter = "home-assistant";
    backend = "systemd";
    findtime = "1d";
    bantime = "1d";
    maxretry = 3;
  };
  environment.etc."fail2ban/filter.d/home-assistant.local".text = ''
    [Definition]
    failregex = .*Login attempt or request with invalid authentication from <HOST>.*$
    journalmatch = CONTAINER_NAME=home-assistant
  '';
}
