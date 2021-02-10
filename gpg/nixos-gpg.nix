{ pkgs, ... }:
with pkgs; {
  imports = [ <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-graphical-gnome.nix> ];

  boot.kernelPackages = linuxPackages_latest;

  services.pcscd.enable = true;
  services.udev.packages = [ yubikey-personalization ];

  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint ];

  environment.systemPackages = [
    gnupg
    keepassx
    libqrencode
    paperkey
    pinentry-curses
    wget
    yubikey-manager
    zbar
  ];

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "curses";
    };
  };
}
