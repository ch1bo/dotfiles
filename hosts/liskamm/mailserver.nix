# NixOs oci-containers port of docker-mailserver
{ config, pkgs, lib, ... }:

let
  boundPort = "8002";
  serverName = "mail.ncoding.at";
in
{
  services.nginx.virtualHosts.${serverName} = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${boundPort}";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 128M;
      '';
    };
  };

  virtualisation.oci-containers.containers = {
    webmail = {
      image = "mailu/rainloop";
      volumes = [
        "/data/rainloop:/var/www/html/data"
      ];
      ports = [ "${boundPort}:80" ];
    };

    mailserver = {
      image = "docker.io/mailserver/docker-mailserver:11.3.1";
      environment = {
        OVERRIDE_HOSTNAME = "${serverName}";
        LOG_LEVEL = "debug";
        ONE_DIR = "1";
        SSL_TYPE = "manual";
        SSL_CERT_PATH="/certs/fullchain.pem";
        SSL_KEY_PATH="/certs/key.pem";
        ENABLE_SPAMASSASSIN = "1";
        SPAMASSASSIN_SPAM_TO_INBOX = "1";
        MOVE_SPAM_TO_JUNK = "1";
        SA_TAG = "1"; # Add header
        SA_TAG2 = "3"; # Add to subject
        SA_KILL = "5.9"; # Move to Junk
        SA_SPAM_SUBJECT = "[SPAM]";
        ENABLE_CLAMAV = "1";
        # TODO: ENABLE_FAIL2BAN="0";
      };
      ports = [
        "25:25"
        "465:465"
        "587:587"
        "993:993"
      ];
      volumes = [
        "/data/mail/data:/var/mail"
        "/data/mail/state:/var/mail-state"
        "/data/mail/config:/tmp/docker-mailserver"
        "${config.security.acme.certs.${serverName}.directory}:/certs:ro"
      ];
      extraOptions = [
        "--cap-add=NET_ADMIN" # for fail2ban
      ];
    };
  };

  systemd.timers."backup-mail" = {
    wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "daily";
        Persistent = true;
        Unit = "backup-mail.service";
      };
  };

  systemd.services."backup-mail" = {
    script = ''
      set -eu
      # Backup
      ${pkgs.gnutar}/bin/tar -czf /backup/mail-$(date -I).tar.gz -C /data/mail config/ data/

      # Rotate backups, keep 7 days
      ${pkgs.findutils}/bin/find /backup/ -maxdepth 1 -mtime +7 -delete;
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "nobody";
    };
  };
}
