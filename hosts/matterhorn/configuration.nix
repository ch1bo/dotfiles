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

  # Network setup
  networking.hostName = "matterhorn";
  networking.hostId = "1ff2d645";
  networking.networkmanager.enable = true;

  # Time zone and internationalisation
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Use custom xsession as display manager
  services.xserver.enable = true;
  services.xserver.displayManager.defaultSession = "user-xsession";
  services.xserver.displayManager.session = [{
    name = "user-xsession";
    manage = "desktop";
    start = ''exec $HOME/.xsession'';
  }];

  # Keyboard setup
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";
  # TODO: WIP - Customization to emulate a 60% keyboard
  services.xserver.extraLayouts.us-60percent = {
    description = "US layout with 60% keyboard layer switches";
    languages = [ "eng" ];
    symbolsFile = ./symbols/us-60percent;
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Bluetooth support (bluez)
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Sound
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    # Bluetooth support
    package = pkgs.pulseaudioFull;
  };
  boot.extraModprobeConfig = ''
    options snd slots=snd-hda-intel
  '';

  # Docker deamon
  virtualisation.docker.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # TODO(SN): move to a notebook module
  # NOTE(SN): added for matterhorn
  services.xserver.libinput.enable = true;

  # Power management
  services.tlp.enable = true;

  # Configuration currently in xsession/default.nix
  services.autorandr = {
    enable = true;
    defaultTarget = "mobile";
  };

  # Synchronizing things between hosts
  services.syncthing = {
    enable = true;
    user = "ch1bo";
    dataDir = "/home/ch1bo";
    openDefaultPorts = true;
  };

  # Smartcard support
  services.pcscd.enable = true;

  # Gnome secrets service
  services.gnome.gnome-keyring.enable = true;

  ## Programs

  # Games
  programs.steam.enable = true;

  # Android debug bridge and other tools
  programs.adb.enable = true;

  # Screensaver / -locker (usses setuid wrapper)
  programs.slock.enable = true;

  # Other things
  environment.systemPackages = with pkgs; [
    git
    vim
    gnome.gnome-disk-utility
    gnome.nautilus
    dconf
    pavucontrol
    htop
    discord
    xournal
    libreoffice
  ];

  ## User configuration

  users.users.ch1bo = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" "adbusers" ];
    uid = 1000;
  };

  nix = let users = [ "root" "ch1bo" ]; in
    {
      trustedUsers = users;
      allowedUsers = users;
    };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
