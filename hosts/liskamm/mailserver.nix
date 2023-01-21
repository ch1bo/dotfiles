# NixOs oci-containers port of docker-mailserver
{ config, pkgs, lib, ... }:

let
  boundPort = "8002";
  networkName = "mail-back";
in
{
  services.nginx.virtualHosts."mail.fk.ncoding.at" = {
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
      # TODO: check using latest version
      image = "runningman84/rainloop:sha256:d43243b7b6f83cb5f75f1e34327d049b868cd143d44cdd3f9edf995d957155bf";
      volumes = [
        "/data/rainloop:/var/www/html/data"
      ];
      environment = {
        PHP_MAX_POST_SIZE = "256M";
        PHP_MAX_UPLOAD_SIZE = "128M";
      };
      ports = [ "${boundPort}:80" ];
    };

    mailserver = {
      image = "docker.io/mailserver/docker-mailserver:8.0.1";
      environment = {
        DMS_DEBUG = "1";
        ENABLE_SPAMASSASSIN = "1";
        SPAMASSASSIN_SPAM_TO_INBOX = "1";
        MOVE_SPAM_TO_JUNK = "1";
        SA_TAG = "1"; # Add header
        SA_TAG2 = "3"; # Add to subject
        SA_KILL = "5.9"; # Move to Junk
        SA_SPAM_SUBJECT = "[SPAM]";
        ENABLE_CLAMAV = "1";
        # TODO: ENABLE_FAIL2BAN="0";
        ONE_DIR = "1";
        SSL_TYPE = "manual";
        SSL_CERT_PATH=/certs/fullchain.pem;
        SSL_KEY_PATH=/certs/key.pem;
      };
      ports = [
        "25:25"
        "143:143"
        "465:465"
        "587:587"
        "993:993"
      ];
      volumes = [
        "/data/mail/data:/var/mail"
        "/data/mail/state:/var/mail-state"
        "/data/mail/config:/tmp/docker-mailserver"
        "${config.security.acme.certs.mail.directory}:/certs:ro"
      ];
      extraOptions = [
        "--cap-add=NET_ADMIN" # for fail2ban
      ];
    };
  };
}
