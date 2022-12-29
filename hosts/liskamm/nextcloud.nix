# Nextcloudu config ported from previous server and
# https://hub.docker.com/_/nextcloud/#running-this-image-with-docker-compose
{ config, pkgs, lib, ... }:

let
  port = "8001";
in
{
  services.nginx.virtualHosts."nextcloud.fk.ncoding.at" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${port}";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 500M;
      '';
    };
  };

  virtualisation.oci-containers.containers.nextcloud = {
    image = "nextcloud:19";
    ports = [ "127.0.0.1:${port}:80" ];
    volumes = [
      "/data/nextcloud/apps:/var/www/html/apps"
      "/data/nextcloud/config:/var/www/html/config"
      "/data/nextcloud/data:/var/www/html/data"
    ];
    environment = {
      REDIS_HOST = "redis";
      OVERWRITEPROTOCOL = "https";
    };
  };

  virtualisation.oci-containers.containers.db = {
    image = "mariadb:10";
    volumes = [
      "/data/db:/var/lib/mysql"
    ];
    environment = {
      MYSQL_RANDOM_ROOT_PASSWORD = "true";
    };
  };

  virtualisation.oci-containers.containers.redis = {
    image = "redis:7";
  };
}
