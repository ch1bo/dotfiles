# Nextcloud config ported from previous server and
# https://hub.docker.com/_/nextcloud/#running-this-image-with-docker-compose
{ config, pkgs, lib, ... }:

let
  version = "30.0.1";
  port = 8001;
  networkName = "nextcloud";
  serverName = "nextcloud.ncoding.at";
in
{
  services.nginx.virtualHosts.${serverName} = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 4G;
        # Increase timeouts for nextcloud
        proxy_connect_timeout 600s;
        proxy_send_timeout 600s;
        proxy_read_timeout 600s;
        fastcgi_send_timeout 600s;
        fastcgi_read_timeout 600s;
      '';
    };
  };

  # XXX: DRY with other docker stacks
  systemd.services."init-docker-network-${networkName}" = {
    description = "Network bridge for nextcloud.";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
    };
    script =
      let dockercli = "${config.virtualisation.docker.package}/bin/docker";
      in ''
        if [[ $(${dockercli} network inspect ${networkName}) == "[]" ]]; then
          ${dockercli} network create ${networkName}
        else
          echo "Docker network ${networkName} already exists"
        fi
      '';
  };

  virtualisation.oci-containers.containers = {
    nextcloud = {
      image = "nextcloud:${version}";
      environment = {
        # XXX: Depends on when the network was created, should create the
        # network more declaratively.
        TRUSTED_PROXIES = "172.20.0.1";
        APACHE_DISABLE_REWRITE_IP = "1";
        OVERWRITEPROTOCOL = "https";
        OVERWRITEHOST = serverName;
        MYSQL_HOST = "nextcloud-db";
        REDIS_HOST = "nextcloud-redis";
      };
      ports = [ "${toString port}:80" ];
      volumes = [
        "/data/nextcloud/apps:/var/www/html/apps"
        "/data/nextcloud/config:/var/www/html/config"
        "/data/nextcloud/data:/var/www/html/data"
      ];
      dependsOn = [ "nextcloud-db" "nextcloud-redis" ];
      extraOptions = [ "--network=${networkName}" ];
    };

    nextcloud-db = {
      image = "mariadb:11.5";
      environment = {
        MYSQL_RANDOM_ROOT_PASSWORD = "true";
        MARIADB_AUTO_UPGRADE = "true";
      };
      volumes = [
        "/data/nextcloud/db:/var/lib/mysql"
      ];
      extraOptions = [ "--network=${networkName}" ];
    };

    nextcloud-redis = {
      image = "redis:7";
      extraOptions = [ "--network=${networkName}" ];
    };
  };

  # Backup to borgbase
  services.borgbackup.jobs.nextcloud = {
    paths = [
      "/data/nextcloud/config"
      "/data/nextcloud/data"
      "/data/nextcloud/nextcloud.sql.gz"
    ];
    doInit = true;
    repo = "n7ixpw3b@n7ixpw3b.repo.borgbase.com:repo";
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat /root/keys/borg/nextcloud.pass";
    };
    environment.BORG_RSH = "ssh -i /root/keys/borg/id_ed25519";
    compression = "auto,lzma";
    startAt = "daily";
    prune.keep = {
      daily = 7; # Keep 7 daily archives
      weekly = 4; # Keep 4 weekly archives
      monthly = 12; # Keep 12 monthly archives
    };
    # Backup nextcloud database
    readWritePaths = [
      "/data/nextcloud"
    ];
    preHook = ''
      MYSQL_PWD=$(${pkgs.gnugrep}/bin/grep dbpassword /data/nextcloud/config/config.php | ${pkgs.gnused}/bin/sed "s/.*dbpassword.*=>.*'\(.*\)',/\1/")
      ${pkgs.docker}/bin/docker exec -e MYSQL_PWD=$MYSQL_PWD db mariadb-dump nextcloud -u oc_ch1bo \
        | ${pkgs.gzip}/bin/gzip \
        > /data/nextcloud/nextcloud.sql.gz
    '';
  };

  # Fail2ban blocking of failed login attempts
  services.fail2ban.enable = true;
  services.fail2ban.jails.nextcloud.settings = {
    enabled = true;
    filter = "nextcloud";
    backend = "auto";
    logpath = "/data/nextcloud/data/nextcloud.log";
    findtime = "1d";
    bantime = "1d";
    maxretry = 3;
  };
  environment.etc."fail2ban/filter.d/nextcloud.local".text = ''
    [Definition]
    _groupsre = (?:(?:,?\s*"\w+":(?:"[^"]+"|\w+))*)
    failregex = ^\{%(_groupsre)s,?\s*"remoteAddr":"<HOST>"%(_groupsre)s,?\s*"message":"Login failed:
                ^\{%(_groupsre)s,?\s*"remoteAddr":"<HOST>"%(_groupsre)s,?\s*"message":"Trusted domain error.
    datepattern = ,?\s*"time"\s*:\s*"%%Y-%%m-%%d[T ]%%H:%%M:%%S(%%z)?"
  '';
}
