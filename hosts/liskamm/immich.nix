# Google photos alternative
#
# Container config ported from
# https://github.com/immich-app/immich/releases/latest/download/docker-compose.yml
#
# Uses a private network to connect server, machine learning, db and redis
# instances together.
#
# Including fail2ban and off-site backups using borgbase.
{ config, pkgs, lib, ... }:
let
  # Check release notes
  # https://github.com/immich-app/immich/releases
  version = "v1.131.3";
  port = 2283; # not exposed
  networkName = "immich";
  DB_DATABASE_NAME = "immich";
  DB_USERNAME = "postgres";
  # NOTE: Database only reachable from local, bridged network
  DB_PASSWORD = "uoa77tynl7jbuiFkr6PhuzUM";
in
{
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
      image = "ghcr.io/immich-app/immich-server:${version}";
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

    immich-machine-learning = {
      image = "ghcr.io/immich-app/immich-machine-learning:${version}";
      volumes = [
        "/data/immich/cache:/cache"
      ];
      extraOptions = [ "--network=${networkName}" ];
    };

    immich-db = {
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

  # Backup to borgbase
  services.borgbackup.jobs.immich = {
    paths = [
      "/data/immich/files"
      "/data/immich/immich.sql.gz"
    ];
    doInit = true;
    repo = "o94wbu5s@o94wbu5s.repo.borgbase.com:repo";
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat /root/keys/borg/immich.pass";
    };
    environment.BORG_RSH = "ssh -i /root/keys/borg/id_ed25519";
    compression = "auto,lzma";
    startAt = "daily";
    prune.keep = {
      daily = 7; # Keep 7 daily archives
      weekly = 4; # Keep 4 weekly archives
      monthly = 12; # Keep 12 monthly archives
    };
    # Backup immich database
    readWritePaths = [
      "/data/immich"
    ];
    preHook = ''
      ${pkgs.docker}/bin/docker exec immich-db pg_dumpall --clean --if-exists --username=${DB_USERNAME} \
        | ${pkgs.gzip}/bin/gzip \
        > /data/immich/immich.sql.gz
    '';
  };

  environment.systemPackages = [
    (pkgs.writeShellScriptBin
      "restore-immich-db"
      ''
        ${pkgs.gzip}/bin/gunzip < /data/immich/immich.sql.gz \
        | ${pkgs.docker}/bin/docker exec -i immich-db psql --username=${DB_USERNAME}
      '')
  ];

  # Fail2ban blocking of failed login attempts
  services.fail2ban.enable = true;
  services.fail2ban.jails.immich.settings = {
    enabled = true;
    filter = "immich";
    backend = "systemd";
    findtime = "1d";
    bantime = "1d";
    maxretry = 3;
  };
  environment.etc."fail2ban/filter.d/immich.local".text = ''
    [Definition]
    failregex = .*Failed login attempt for user.+from ip address\s?<ADDR>
    journalmatch = CONTAINER_NAME=immich-server
  '';
}
