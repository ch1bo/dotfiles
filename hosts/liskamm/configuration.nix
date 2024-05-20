{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./nextcloud.nix
    ./mailserver.nix
    ./hydraw.nix
    ./backup.nix
    ./home-assistant.nix
  ];

  # At least the nvidia drivers are proprietary
  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];

  networking.hostName = "liskamm";
  networking.hostId = "24c2d71d"; # required for ZFS
  networking.networkmanager.enable = true;

  networking.firewall.enable = false;

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

  # ZFS
  # 4GB max ARC cache
  boot.kernelParams = [ "zfs.zfs_arc_max=4294967296" ];
  services.zfs.autoScrub.enable = true;
  # Auto-snapshot all marked datasets, e.g
  # zfs set com.sun:auto-snapshot=true root/safe/home
  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoSnapshot.flags = "-k -p --utc";

  ## ncoding.at

  # Port of existing services
  security.acme.acceptTerms = true;
  security.acme.defaults.email = "webmaster@ncoding.at";
  services.nginx.enable = true;
  services.nginx.recommendedProxySettings = true;
  services.nginx.virtualHosts."www.ncoding.at" =
    let
      # REVIEW: Maybe move ncoding.at sources into this repository
      ncoding-source = builtins.fetchTarball {
        url = https://github.com/ch1bo/ncoding.at/archive/609ac5316b233bb92ac7dfa601c8db83629f63f1.tar.gz;
        sha256 = "0sfb9llgx572fndbx4h2s8gykhkcg8ap1yixhakd8js7mrykmr6i";
      };
    in
    {
      serverAliases = [ "ncoding.at" ];
      forceSSL = true;
      enableACME = true;
      root = "${ncoding-source}/web/html";
    };

  ## Programs

  # Other things
  environment.systemPackages = with pkgs; [
    git
    vim
    dconf
    gptfdisk
    # monitoring
    htop
    btop
    smartmontools
    # nix tools
    nix-output-monitor
    nvd
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
  # Increase inotify watches for syncthing - Dynamic since kernel v5.11
  # https://github.com/torvalds/linux/commit/92890123749bafc317bbfacbe0a62ce08d78efb7
  boot.kernel.sysctl."fs.inotify.max_user_watches" = lib.mkIf
    (config.boot.kernelPackages.kernel.kernelOlder "5.11")
    1048576; # instead of 8192

  ## User configuration

  users.users.ch1bo = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    uid = 1000;
  };

  nix = let users = [ "root" "ch1bo" ]; in
    {
      settings.trusted-users = users;
      settings.allowed-users = users;
      # Use upcoming 'nix flake' and updated other commands
      package = pkgs.nixUnstable;
      extraOptions = ''
        experimental-features = nix-command flakes repl-flake
        allow-import-from-derivation = true
        accept-flake-config = true
      '';
      # Prime nix registry with same nixpkgs as system built from
      registry.nixpkgs.flake = inputs.nixpkgs;
    };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
