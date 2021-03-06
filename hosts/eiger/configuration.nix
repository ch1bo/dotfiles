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

  networking.hostName = "eiger";
  networking.hostId = "1caa41c5"; # required for ZFS
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";
  time.hardwareClockInLocalTime = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp6s0.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services.xserver.enable = true;
  services.xserver.displayManager.defaultSession = "user-xsession";
  services.xserver.displayManager.session = [{
    name = "user-xsession";
    manage = "desktop";
    start = ''exec $HOME/.xsession'';
  }];

  # Use nvidia drivers
  services.xserver.videoDrivers = [ "nvidia" ];

  # Disable syscalls to 32bit programs because required by some games (via wine)
  # TODO(SN): move to a gaming module
  boot.kernel.sysctl = {
    "abi.vsyscall32" = 0;
  };

  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

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

  # Printing
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint ];

  # Scanning
  hardware.sane.enable = true;

  # Docker deamon
  virtualisation.docker.enable = true;

  # Access cryptos
  hardware.ledger.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # TODO(SN): move to a notebook module
  # services.xserver.libinput.enable = true;

  ## Programs

  # Including opengl fixes
  programs.steam.enable = true;

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
    ntfsprogs
    system-config-printer
  ];

  ## Services

  # Smartcard support
  services.pcscd.enable = true;

  # TODO(SN): move to a notebook module
  # services.autorandr = {
  #   enable = true;
  #   defaultTarget = "mobile";
  # };

  # Gnome secrets service
  services.gnome.gnome-keyring.enable = true;

  ## User configuration

  users.users.ch1bo = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" "scanner" ];
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
