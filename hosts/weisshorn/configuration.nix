{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/user.nix
    ../../modules/nix-tools.nix
    ./zfs.nix
    ./syncthing.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "weisshorn";
  networking.hostId = "18b70f8d"; # required for ZFS
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;

  time.timeZone = "Europe/Vaduz";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "de_LI.UTF-8";
    LC_IDENTIFICATION = "de_LI.UTF-8";
    LC_MEASUREMENT = "de_LI.UTF-8";
    LC_MONETARY = "de_LI.UTF-8";
    LC_NAME = "de_LI.UTF-8";
    LC_NUMERIC = "de_LI.UTF-8";
    LC_PAPER = "de_LI.UTF-8";
    LC_TELEPHONE = "de_LI.UTF-8";
    LC_TIME = "de_LI.UTF-8";
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  environment.systemPackages = with pkgs; [
    git
    vim
    dconf
    gptfdisk
    # monitoring
    btop
    smartmontools
    # nix tools
    nix-output-monitor
    nvd
  ];

  # Finally, this is me 
  user.name = "ch1bo";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?

}
