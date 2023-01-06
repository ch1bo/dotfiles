# Nextcloud config ported from previous server and
# https://hub.docker.com/_/nextcloud/#running-this-image-with-docker-compose
{ config, pkgs, lib, ... }:

let
  nextcloudPort = "8001";
in
{
  virtualisation.arion.backend = "docker";

  services.nginx.virtualHosts."nextcloud.fk.ncoding.at" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${nextcloudPort}";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 500M;
      '';
    };
  };

  virtualisation.arion.projects.ncoding.settings = {
    networks.back.driver = "bridge";

    services.nextcloud.service = {
      image = "nextcloud:19";
      environment = {
        # REDIS_HOST = "redis";
        OVERWRITEPROTOCOL = "https";
      };
      ports = [ "${nextcloudPort}:80" ];
      volumes = [
        "/data/nextcloud/apps:/var/www/html/apps"
        "/data/nextcloud/config:/var/www/html/config"
        "/data/nextcloud/data:/var/www/html/data"
      ];
      links = [
        "db:mysql"
        "redis:redis"
      ];
      networks = [ "back" ];
      restart = "always";
    };

    services.db.service = {
      image = "mariadb:10.4";
      environment = {
        MYSQL_RANDOM_ROOT_PASSWORD = "true";
      };
      volumes = [
        "/data/db:/var/lib/mysql"
      ];
      networks = [ "back" ];
      restart = "always";
    };

    services.redis.service = {
      image = "redis:7";
      networks = [ "back" ];
    };
  };

}
