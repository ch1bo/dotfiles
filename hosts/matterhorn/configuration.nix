{ config, pkgs, inputs, system, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/obsidian.nix
    ../../modules/dygma-raise2.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Kernel configuration
  boot.kernelParams = [
    # 4GB max ARC cache
    "zfs.zfs_arc_max=4294967296"
  ];
  boot.zfs.forceImportRoot = false;
  boot.zfs.allowHibernation = true; # safe because swap is not on zfs
  # Auto-snapshot all marked datasets, e.g
  # zfs set com.sun:auto-snapshot=true root/safe/home
  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoSnapshot.flags = "-k -p --utc";

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
  services.displayManager.defaultSession = "user-xsession";
  services.xserver.displayManager.session = [{
    name = "user-xsession";
    manage = "desktop";
    start = "exec $HOME/.xsession";
  }];

  # Keyboard setup
  services.xserver.xkb.layout = "us";
  services.xserver.xkb.options = "eurosign:e";
  # TODO: WIP - Customization to emulate a 60% keyboard
  services.xserver.xkb.extraLayouts.us-60percent = {
    description = "US layout with 60% keyboard layer switches";
    languages = [ "eng" ];
    symbolsFile = ./symbols/us-60percent;
  };

  # Enable mDNS (a.k.a bonjour)
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  # Printing
  services.printing.enable = true;
  # services.printing.drivers = [ pkgs.gutenprint ];
  services.printing.drivers = [ pkgs.hplipWithPlugin ];
  # Prevent CVE-2024-47176
  # https://www.evilsocket.net/2024/09/26/Attacking-UNIX-systems-via-CUPS-Part-I/
  systemd.services.cups-browsed.enable = false;

  # Scanning
  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.sane-airscan ];
  hardware.sane.disabledDefaultBackends = [ "escl" ];

  # Bluetooth support (bluez)
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Docker deamon
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Access cryptos
  hardware.ledger.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # TODO(SN): move to a notebook module
  services.libinput.enable = true;

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

  # Auto-mount mtp devices
  services.gvfs.enable = true;

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
    atool
    unzip
    websocat
    pdftk
    imagemagick
    inkscape
    gimp
    obs-studio
    ffmpeg
    mplayer
    ledger-live-desktop
    telegram-desktop
    # system monitoring
    lm_sensors
    btop
    htop
    powertop
    gnome-disk-utility
    nautilus
    simple-scan
    dconf
    pavucontrol
    discord
    element-desktop
    xournal
    libreoffice
    portfolio
    eva
    bind.dnsutils
    system-config-printer
    usbutils
    # nix tools
    nix-output-monitor
    nvd
    # printing
    cups-filters
  ];

  fonts.packages = [
    pkgs.fira-code
    pkgs.hasklig
    (pkgs.nerdfonts.override {
      fonts = [
        "FiraCode"
        "Hasklig"
        "NerdFontsSymbolsOnly"
      ];
    })
    pkgs.roboto
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
      extraOptions = ''
        experimental-features = nix-command flakes
        allow-import-from-derivation = true
        accept-flake-config = true
      '';
      # Prime nix registry with same nixpkgs as system built from
      registry.nixpkgs.flake = inputs.nixpkgs;
    };

  # At least spotify is proprietary
  nixpkgs.config.allowUnfree = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
