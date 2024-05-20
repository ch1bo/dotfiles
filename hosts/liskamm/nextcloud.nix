# Nextcloud config ported from previous server and
# https://hub.docker.com/_/nextcloud/#running-this-image-with-docker-compose
{ config, pkgs, lib, ... }:

let
  nextcloudPort = "8001";
  networkName = "nextcloud-back";
  serverName = "nextcloud.ncoding.at";
in
{
  services.nginx.virtualHosts.${serverName} = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${nextcloudPort}";
      proxyWebsockets = true;
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
      image = "nextcloud:29";
      environment = {
        TRUSTED_PROXIES = "172.18.0.1";
        APACHE_DISABLE_REWRITE_IP = "1";
        OVERWRITEPROTOCOL = "https";
        OVERWRITEHOST = serverName;
        MYSQL_HOST = "db";
        REDIS_HOST = "redis";
      };
      ports = [ "${nextcloudPort}:80" ];
      volumes = [
        "/data/nextcloud/apps:/var/www/html/apps"
        "/data/nextcloud/config:/var/www/html/config"
        "/data/nextcloud/data:/var/www/html/data"
      ];
      extraOptions = [ "--network=${networkName}" ];
    };

    db = {
      image = "mariadb:10.10";
      environment = {
        MYSQL_RANDOM_ROOT_PASSWORD = "true";
        MARIADB_AUTO_UPGRADE = "true";
      };
      volumes = [
        "/data/db:/var/lib/mysql"
      ];
      extraOptions = [ "--network=${networkName}" ];
    };

    redis = {
      image = "redis:7";
      extraOptions = [ "--network=${networkName}" ];
    };
  };
}
