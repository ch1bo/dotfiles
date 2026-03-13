{ config, pkgs, ... }:

{
  virtualisation.libvirtd.enable = true;

  # Allow VM management
  users.groups.libvirtd.members = [ config.user.name ];
  users.groups.kvm.members = [ config.user.name ];

  environment.systemPackages = with pkgs; [
    gnome-boxes # VM frontend
    dnsmasq # VM networking
  ];
}
