{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/user.nix
    ../../modules/docker.nix
    ./ncoding.nix
    ./laendlefinder.nix
    ./nextcloud.nix
    ./mailserver.nix
    ./hydraw.nix
    ./home-assistant.nix
    ./immich.nix
    ./vaultwarden.nix
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
  networking.firewall.enable = true;

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

  # ZFS
  # 4GB max ARC cache
  boot.kernelParams = [ "zfs.zfs_arc_max=4294967296" ];
  services.zfs.autoScrub.enable = true;
  # Auto-snapshot all marked datasets, e.g
  # zfs set com.sun:auto-snapshot=true root/safe/home
  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoSnapshot.flags = "-k -p --utc";

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

  # Fail2ban
  services.fail2ban.enable = true;
  services.fail2ban.ignoreIP = [
    "192.168.0.0/16"
    "212.186.186.46/24"
  ];

  # SSH
  programs.ssh.package = pkgs.openssh;
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
    user = config.user.name;
    dataDir = config.user.home;
    openDefaultPorts = true;
  };

  # TODO: configure gui password via sops or age
  services.syncthing.guiAddress = "0.0.0.0:8384";
  networking.firewall.allowedTCPPorts = [ 8384 ];

  # Increase inotify watches for syncthing - Dynamic since kernel v5.11
  # https://github.com/torvalds/linux/commit/92890123749bafc317bbfacbe0a62ce08d78efb7
  boot.kernel.sysctl."fs.inotify.max_user_watches" =
    lib.mkIf (config.boot.kernelPackages.kernel.kernelOlder "5.11") 1048576; # instead of 8192

  # Finally, this is me
  user.name = "ch1bo";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
