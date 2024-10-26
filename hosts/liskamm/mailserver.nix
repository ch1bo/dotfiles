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
    # FIXME: webmail container currently broken
    # Feb 11 20:36:04 liskamm docker-webmail-start[2795016]: Traceback (most recent call last):
    # Feb 11 20:36:04 liskamm docker-webmail-start[2795016]:   File "/start.py", line 13, in <module>
    # Feb 11 20:36:04 liskamm docker-webmail-start[2795016]:     os.environ["MAX_FILESIZE"] = str(int(int(os.environ.get("MESSAGE_SIZE_LIMIT"))*0.66/1048576))
    # Feb 11 20:36:04 liskamm docker-webmail-start[2795016]: TypeError: int() argument must be a string, a bytes-like object or a number, not 'NoneType'
    # webmail = {
    #   image = "mailu/rainloop";
    #   volumes = [
    #     "/data/rainloop:/var/www/html/data"
    #   ];
    #   ports = [ "${boundPort}:80" ];
    # };

    mailserver = {
      image = "docker.io/mailserver/docker-mailserver:11.3.1";
      environment = {
        OVERRIDE_HOSTNAME = "${serverName}";
        LOG_LEVEL = "trace";
        ONE_DIR = "0"; # TODO: broken? as it can't replace /var/spool/postfix?
        SSL_TYPE = "manual";
        SSL_CERT_PATH = "/certs/fullchain.pem";
        SSL_KEY_PATH = "/certs/key.pem";
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
