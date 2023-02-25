{ config, pkgs, lib, inputs, system, ... }:
let
  unstable = import inputs.nixpkgs-unstable {
    config = config.nixpkgs.config;
    inherit system;
  };
in
{
  imports = [ ./hardware-configuration.nix ];

  # At least the nvidia drivers are proprietary
  nixpkgs.config.allowUnfree = true;
  # XXX: zfs kernel module still marked as broken
  nixpkgs.config.allowBroken = true;
  nixpkgs.overlays = [
    inputs.emacsOverlay.overlays.emacs
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Network setup
  networking.hostName = "matterhorn";
  networking.hostId = "1ff2d645";
  networking.networkmanager.enable = true;
  networking.enableIPv6 = false;

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
    start = "exec $HOME/.xsession";
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

  # Printing
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint ];

  # Scanning
  hardware.sane.enable = true;

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
  services.tlp.settings = {
    CPU_SCALING_GOVERNOR_ON_AC = "performance";
    CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";
  };

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
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  # Remote administration / support
  services.teamviewer.enable = true;

  # Screensaver / -locker (usses setuid wrapper)
  programs.slock.enable = true;

  # Other things
  environment.systemPackages = with pkgs; [
    git
    vim
    gnome.gnome-disk-utility
    gnome.nautilus
    gnome.simple-scan
    dconf
    pavucontrol
    htop
    unstable.discord
    xournal
    libreoffice
    unstable.portfolio
    eva
    bind.dnsutils
    system-config-printer
    usbutils
  ];

  ## User configuration

  users.users.ch1bo = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" "adbusers" ];
    uid = 1000;
  };

  nix =
    let users = [ "root" "ch1bo" ];
    in {
      settings.trusted-users = users;
      settings.allowed-users = users;
      # Use upcoming 'nix flake' and updated other commands
      package = pkgs.nixUnstable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
