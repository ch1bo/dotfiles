# Google photos alternative
#
# Container config ported from
# https://github.com/immich-app/immich/releases/latest/download/docker-compose.yml
#
# TODO: Add machine learning?

{ config, pkgs, lib, ... }:

let
  port = 2283;
  networkName = "immich";
  DB_DATABASE_NAME = "immich";
  # FIXME: use a domain socket without auth instead (no network access)
  DB_USERNAME = "postgres";
  DB_PASSWORD = "postgres";
in
{
  networking.firewall.allowedTCPPorts = [ port ];

  services.nginx.virtualHosts."photos.ncoding.at" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 4G;
      '';
    };
  };

  # XXX: DRY with other docker stacks
  systemd.services."init-docker-network-${networkName}" = {
    description = "Network bridge for ${networkName}.";
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
    immich-server = {
      image = "ghcr.io/immich-app/immich-server:v1.118.2";
      environment = {
        TZ = "Europe/Vienna";
        DB_HOSTNAME = "immich-db";
        inherit DB_DATABASE_NAME DB_USERNAME DB_PASSWORD;
        REDIS_HOSTNAME = "immich-redis";
      };
      ports = [ "127.0.0.1:${toString port}:${toString port}" ];
      volumes = [
        "/data/immich/files:/usr/src/app/upload"
        "/etc/localtime:/etc/localtime:ro"
      ];
      dependsOn = [ "immich-db" "immich-redis" ];
      extraOptions = [ "--network=${networkName}" ];
    };

    immich-db = {
      # NOTE: Special versin of postgres
      image = "docker.io/tensorchord/pgvecto-rs:pg14-v0.2.0@sha256:90724186f0a3517cf6914295b5ab410db9ce23190a2d9d0b9dd6463e3fa298f0";
      environment = {
        POSTGRES_PASSWORD = DB_PASSWORD;
        POSTGRES_USER = DB_USERNAME;
        POSTGRES_DB = DB_DATABASE_NAME;
        POSTGRES_INITDB_ARGS = "--data-checksums";
      };
      volumes = [
        "/data/immich/db:/var/lib/postgresql/data"
      ];
      cmd =
        [
          "postgres"
          "-c"
          "shared_preload_libraries=vectors.so"
          "-c"
          "search_path=\"$$user\", public, vectors"
          "-c"
          "logging_collector=on"
          "-c"
          "max_wal_size=2GB"
          "-c"
          "shared_buffers=512MB"
          "-c"
          "wal_compression=on"
        ];
      extraOptions = [ "--network=${networkName}" ];
    };

    immich-redis = {
      image = "docker.io/redis:6.2-alpine@sha256:2ba50e1ac3a0ea17b736ce9db2b0a9f6f8b85d4c27d5f5accc6a416d8f42c6d5";
      extraOptions = [ "--network=${networkName}" ];
    };
  };

}
