{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/user.nix
    ../../modules/desktop.nix
    ../../modules/nix-tools.nix
    ../../modules/docker.nix
    ../../modules/obsidian.nix
    ../../modules/dygma-raise2.nix
    ../../modules/logitech-mouse.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Kernel configuration
  boot.kernelParams = [
    # 12GB max ARC cache
    "zfs.zfs_arc_max=12884901888"
  ];
  boot.zfs.forceImportRoot = false;
  boot.zfs.allowHibernation = true; # safe because swap is not on zfs
  # Auto-snapshot all marked datasets, e.g
  # zfs set com.sun:auto-snapshot=true root/safe/home
  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoSnapshot.flags = "-k -p --utc";

  # Network setup
  networking.hostName = "eiger";
  networking.hostId = "1caa41c5"; # required for ZFS
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";
  time.hardwareClockInLocalTime = true;

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;

  # Wake on LAN
  networking = {
    interfaces.eno1.wakeOnLan.enable = true;
    firewall.allowedUDPPorts = [ 9 ];
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services.xserver.enable = true;
  services.displayManager.defaultSession = "user-xsession";
  services.xserver.displayManager.session = [
    {
      name = "user-xsession";
      manage = "desktop";
      start = "exec $HOME/.xsession";
    }
  ];

  # Use nvidia drivers
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.open = true;

  # Disable syscalls to 32bit programs because required by some games (via wine)
  # TODO(SN): move to a gaming module
  boot.kernel.sysctl = {
    "abi.vsyscall32" = 0;
  };

  # Configure keymap in X11
  services.xserver.xkb.layout = "us";
  services.xserver.xkb.options = "eurosign:e";

  # Bluetooth support (bluez)
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Enable mDNS (a.k.a bonjour)
  services.avahi.enable = true;

  # Printing
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint ];
  # Prevent CVE-2024-47176
  # https://www.evilsocket.net/2024/09/26/Attacking-UNIX-systems-via-CUPS-Part-I/
  systemd.services.cups-browsed.enable = false;

  # Scanning
  hardware.sane.enable = true;

  # Access cryptos
  hardware.ledger.enable = true;

  ## Programs

  # Including opengl fixes
  programs.steam.enable = true;

  # Screensaver / -locker (usses setuid wrapper)
  programs.slock.enable = true;

  # Android tools
  programs.adb.enable = true;

  fonts.packages = [
    pkgs.fira-code
    pkgs.hasklig
    pkgs.nerd-fonts.fira-code
    pkgs.nerd-fonts.hasklug
    pkgs.nerd-fonts.symbols-only
  ];

  ## Services

  # SSH
  programs.ssh.package = pkgs.openssh;
  services.openssh = {
    enable = true;
    # GPG agent forwarding
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
  };

  # Required for automount (udiskie)
  services.udisks2.enable = true;

  # Smartcard support
  services.pcscd.enable = true;

  # Gnome secrets service
  services.gnome.gnome-keyring.enable = true;

  # Synchronizing things between hosts
  services.syncthing = {
    enable = true;
    user = config.user.name;
    dataDir = config.user.home;
    openDefaultPorts = true;
  };

  # At least spotify is proprietary
  nixpkgs.config.allowUnfree = true;

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
