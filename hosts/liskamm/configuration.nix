# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # At least the nvidia drivers are proprietary
  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];

  # Dynamic since kernel v5.11
  # https://github.com/torvalds/linux/commit/92890123749bafc317bbfacbe0a62ce08d78efb7
  boot.kernel.sysctl."fs.inotify.max_user_watches" = lib.mkIf
    (config.boot.kernelPackages.kernel.kernelOlder "5.11")
    1048576; # instead of 8192

  networking.hostName = "liskamm";
  networking.hostId = "24c2d71d"; # required for ZFS
  networking.networkmanager.enable = true;

  networking.firewall.enable = false;
  # Hydra node
  #networking.firewall.allowedTCPPorts = [ 5001 ];

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Docker deamon
  virtualisation.docker.enable = true;

  ## ncoding.at

  # Port of existing services
  security.acme.acceptTerms = true;
  security.acme.email = "webmaster@ncoding.at";
  services.nginx =
    let
      # REVIEW: Maybe move ncoding.at sources into this repository
      ncoding-source = builtins.fetchTarball {
        url = https://github.com/ch1bo/ncoding.at/archive/609ac5316b233bb92ac7dfa601c8db83629f63f1.tar.gz;
      };
    in
    {
      enable = true;
      virtualHosts."fk.ncoding.at" = {
        serverAliases = [ "www.fk.ncoding.at" ];
        forceSSL = true;
        enableACME = true;
        root = "${ncoding-source}/web/html";
      };

      virtualHosts."photos.fk.ncoding.at" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:2342";
          extraConfig =
            #   # required when the target is also TLS server with multiple hosts
            #   "proxy_ssl_server_name on;" +
            #   # required when the server wants to use HTTP Authentication
            #   "proxy_pass_header Authorization;" +
            #   proxy_buffering off;
            #   proxy_http_version 1.1;
            #   proxy_set_header Upgrade $http_upgrade;
            #   proxy_set_header Connection "upgrade";
            ''
              client_max_body_size 500M;
            '';
        };
      };
    };

  # New photo management
  virtualisation.oci-containers.containers = {
    # Ported from https://dl.photoprism.app/docker/docker-compose.yml
    photoprism = {
      image = "photoprism/photoprism:latest";
      ports = [ "127.0.0.1:2342:2342" ];
      volumes = [
        "/data/pictures:/photoprism/originals"
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
  };

  ## Programs

  # Other things
  environment.systemPackages = with pkgs; [
    git
    vim
    dconf
    gptfdisk
    htop
  ];

  ## Services

  # SSH
  services.openssh = {
    enable = true;
    # GPG agent forwarding
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
  };

  # Synchronizing things between hosts
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = "ch1bo";
    configDir = "/home/ch1bo/.config/syncthing";
    guiAddress = "0.0.0.0:8384";
  };

  ## User configuration

  users.users.ch1bo = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    uid = 1000;
  };

  nix = let users = [ "root" "ch1bo" ]; in
    {
      trustedUsers = users;
      allowedUsers = users;
      # Use upcoming 'nix flake' and updated other commands
      package = pkgs.nixUnstable;
      extraOptions = ''
        experimental-features = nix-command flakes
        allow-import-from-derivation = true
      '';
    };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
