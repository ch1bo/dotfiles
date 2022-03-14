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

  networking.hostName = "liskamm";
  networking.hostId = "24c2d71d"; # required for ZFS
  networking.networkmanager.enable = true;

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

  ## Programs
  
  # Other things
  environment.systemPackages = with pkgs; [
    git
    vim
    dconf
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
