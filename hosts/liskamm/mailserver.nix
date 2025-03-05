# NixOS oci-containers port of docker-mailserver
#
# Used basic configuration example as starting point
# https://docker-mailserver.github.io/docker-mailserver/latest/examples/tutorials/basic-installation/#a-basic-example-with-relevant-environmental-variables
#
# Documentation of environment variables
# https://docker-mailserver.github.io/docker-mailserver/latest/config/environment/
#
# Including proxied roundcube webmail client, fail2ban and off-site backups using borgbase.
{ config, pkgs, lib, ... }:
let
  # Check release notes
  # https://github.com/docker-mailserver/docker-mailserver/releases
  version = "15.0.0";
  serverName = "mail.ncoding.at";
  webmailPort = 8000; # not exposed
  imapPort = 993;
in
{
  # Exposes the mail server ports
  networking.firewall.allowedTCPPorts = [ 25 465 993 ];

  # Exposes the webmail client
  services.nginx.virtualHosts.${serverName} = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString webmailPort}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
    };
  };

  virtualisation.oci-containers.containers = {
    mailserver = {
      image = "docker.io/mailserver/docker-mailserver:${version}";
      hostname = serverName;
      ports = [
        "25:25" # smtp transfer
        "465:465" # smtp submission
        "993:993" # imap retrieval
        "11334:11334" # rspamd webui
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
        ENABLE_OPENDKIM = "0";
        ENABLE_OPENDMARC = "0";
        ENABLE_POLICYD_SPF = "0";
        ENABLE_AMAVIS = "0";
        ENABLE_SPAMASSASSIN = "0";
        ENABLE_POSTGREY = "0";
        RSPAMD_GREYLISTING = "1";
        # Rspam config
        RSPAMD_LEARN = "1";
        RSPAMD_NEURAL = "1";
        MOVE_SPAM_TO_JUNK = "1";
        MARK_SPAM_AS_READ = "1";
        SPAM_SUBJECT = "[SPAM]";
      };
      extraOptions = [
        "--cap-add=NET_ADMIN" # for fail2ban
      ];
    };

    webmail = {
      image = "docker.io/roundcube/roundcubemail:latest";
      ports = [ "${toString webmailPort}:80" ];
      environment = {
        ROUNDCUBEMAIL_DEFAULT_HOST = "ssl://mail.ncoding.at";
        ROUNDCUBEMAIL_DEFAULT_PORT = "993";
        ROUNDCUBEMAIL_SMTP_SERVER = "ssl://mail.ncoding.at";
        ROUNDCUBEMAIL_SMTP_PORT = "465";
        ROUNDCUBEMAIL_USERNAME_DOMAIN = "ncoding.at";
      };
      dependsOn = [ "mailserver" ];
    };
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
