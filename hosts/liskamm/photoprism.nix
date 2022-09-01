# New photo management

{ config, pkgs, lib, ... }:

{
  services.nginx.virtualHosts."photos.fk.ncoding.at" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:2342";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 500M;
      '';
    };
  };

  # Ported from https://dl.photoprism.app/docker/docker-compose.yml
  virtualisation.oci-containers.containers.photoprism = {
    image = "photoprism/photoprism:latest";
    ports = [ "127.0.0.1:2342:2342" ];
    volumes = [
      "/home/ch1bo/pictures:/photoprism/originals"
      "/data/photoprism-storage:/photoprism/storage"
    ];
    environment = {
      PHOTOPRISM_ADMIN_PASSWORD = "ouHTWxYXtvX1rEW"; # INITIAL PASSWORD FOR "admin" USER, MINIMUM 8 CHARACTERS
      PHOTOPRISM_AUTH_MODE = "password"; # authentication mode (public, password)
      PHOTOPRISM_SITE_URL = "https://photos.fk.ncoding.at"; # public server URL incl http:// or https:// and /path, :port is optional
      PHOTOPRISM_HTTP_PORT = "2342";
      PHOTOPRISM_ORIGINALS_LIMIT = "5000"; # file size limit for originals in MB (increase for high-res video)
      PHOTOPRISM_HTTP_COMPRESSION = "gzip"; # improves transfer speed and bandwidth utilization (none or gzip)
      PHOTOPRISM_LOG_LEVEL = "info"; # log level = trace, debug, info, warning, error, fatal, or panic
      PHOTOPRISM_READONLY = "false"; # do not modify originals directory (reduced functionality)
      PHOTOPRISM_EXPERIMENTAL = "false"; # enables experimental features
      PHOTOPRISM_DISABLE_CHOWN = "true"; # disables updating storage permissions via chmod and chown on startup
      PHOTOPRISM_DISABLE_WEBDAV = "false"; # disables built-in WebDAV server
      PHOTOPRISM_DISABLE_SETTINGS = "false"; # disables settings UI and API
      PHOTOPRISM_DISABLE_TENSORFLOW = "false"; # disables all features depending on TensorFlow
      PHOTOPRISM_DISABLE_FACES = "false"; # disables face detection and recognition (requires TensorFlow)
      PHOTOPRISM_DISABLE_CLASSIFICATION = "false"; # disables image classification (requires TensorFlow)
      PHOTOPRISM_DISABLE_RAW = "false"; # disables indexing and conversion of RAW files
      PHOTOPRISM_RAW_PRESETS = "false"; # enables applying user presets when converting RAW files (reduces performance)
      PHOTOPRISM_JPEG_QUALITY = "85"; # a higher value increases the quality and file size of JPEG images and thumbnails (25-100)
      PHOTOPRISM_DETECT_NSFW = "false"; # automatically flags photos as private that MAY be offensive (requires TensorFlow)
      PHOTOPRISM_UPLOAD_NSFW = "true"; # allows uploads that MAY be offensive (no effect without TensorFlow)
      PHOTOPRISM_DATABASE_DRIVER = "sqlite"; # SQLite is an embedded database that doesn't require a server
      PHOTOPRISM_DATABASE_NAME = "photoprism"; # MariaDB or MySQL database schema name
      PHOTOPRISM_DATABASE_USER = "photoprism"; # MariaDB or MySQL database user name
      PHOTOPRISM_DATABASE_PASSWORD = "insecure"; # MariaDB or MySQL database user password
      PHOTOPRISM_SITE_CAPTION = "AI-Powered Photos App";
      PHOTOPRISM_SITE_DESCRIPTION = ""; # meta site description
      PHOTOPRISM_SITE_AUTHOR = ""; # meta site author
    };
    workdir = "/photoprism";
  };

}
