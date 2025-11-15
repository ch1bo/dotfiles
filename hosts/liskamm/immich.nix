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
  version = "v2.2.2";
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

        # Allow CORS for the official hosted instance
        if ($request_method = 'OPTIONS') {
            add_header 'Access-Control-Allow-Origin' 'https://ch1bo.github.io' always;
            add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, OPTIONS' always;
            add_header 'Access-Control-Allow-Headers' 'x-api-key, Content-Type, Accept' always;
            add_header 'Access-Control-Max-Age' 1728000;
            add_header 'Content-Type' 'text/plain charset=UTF-8';
            add_header 'Content-Length' 0;
            return 204;
        }

        add_header 'Access-Control-Allow-Origin' 'https://ch1bo.github.io' always;
        add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, OPTIONS' always;
        add_header 'Access-Control-Allow-Headers' 'x-api-key, Content-Type, Accept' always;
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
      image = "ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0@sha256:32324a2f41df5de9efe1af166b7008c3f55646f8d0e00d9550c16c9822366b4a";
      environment = {
        POSTGRES_PASSWORD = DB_PASSWORD;
        POSTGRES_USER = DB_USERNAME;
        POSTGRES_DB = DB_DATABASE_NAME;
        POSTGRES_INITDB_ARGS = "--data-checksums";
      };
      volumes = [
        "/data/immich/db:/var/lib/postgresql/data"
      ];
      extraOptions = [ "--network=${networkName}" ];
    };

    immich-redis = {
      image = "docker.io/valkey/valkey:8-bookworm@sha256:facc1d2c3462975c34e10fccb167bfa92b0e0dbd992fc282c29a61c3243afb11";
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
