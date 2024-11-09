# NixOS oci-containers port of docker-mailserver
#
# Used basic configuration example as starting point
# https://docker-mailserver.github.io/docker-mailserver/latest/examples/tutorials/basic-installation/#a-basic-example-with-relevant-environmental-variables
#
# Documentation of environment variables
# https://docker-mailserver.github.io/docker-mailserver/latest/config/environment/
#
# Off-site backups using borgbase.
{ config, pkgs, lib, ... }:
let
  version = "14.0.0";
  serverName = "mail.ncoding.at";
in
{
  services.nginx.virtualHosts.${serverName} = {
    forceSSL = true;
    enableACME = true;
  };

  networking.firewall.allowedTCPPorts = [ 25 465 587 993 ];

  virtualisation.oci-containers.containers.mailserver = {
    image = "docker.io/mailserver/docker-mailserver:${version}";
    hostname = serverName;
    ports = [
      "25:25"
      "465:465"
      "587:587"
      "993:993"
    ];
    volumes = [
      "/data/mail/data:/var/mail"
      "/data/mail/state:/var/mail-state"
      "/data/mail/logs/:/var/log/mail"
      "/data/mail/config:/tmp/docker-mailserver"
      "/etc/localtime:/etc/localtime:ro"
      "${config.security.acme.certs.${serverName}.directory}:/certs:ro"
    ];
    environment = {
      SSL_TYPE = "manual";
      SSL_CERT_PATH = "/certs/fullchain.pem";
      SSL_KEY_PATH = "/certs/key.pem";
      ENABLE_FAIL2BAN = "1";
      ENABLE_RSPAMD = "1";
      ENABLE_CLAMAV = "1";
      # Rspamd replaces the following
      ENABLE_POLICYD_SPF = "0";
      ENABLE_AMAVIS = "0";
      ENABLE_SPAMASSASSIN = "0";
      RSPAMD_GREYLISTING = "1";
      RSPAMD_LEARN = "1";
      SPAM_SUBJECT = "[SPAM]";
    };
    extraOptions = [
      "--cap-add=NET_ADMIN" # for fail2ban
    ];
  };

  # Backup to borgbase
  services.borgbackup.jobs.mail = {
    paths = [ "/data/mail" ];
    doInit = true;
    repo = "smsua417@smsua417.repo.borgbase.com:repo";
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat /root/keys/borg/mail.pass";
    };
    # TODO: use agenix
    # https://github.com/ryantm/agenix
    environment.BORG_RSH = "ssh -i /root/keys/borg/id_ed25519";
    compression = "auto,lzma";
    startAt = "daily";
    prune.keep = {
      daily = 7; # Keep 7 daily archives
      weekly = 4; # Keep 4 weekly archives
      monthly = 12; # Keep 12 monthly archives
    };
  };
}
