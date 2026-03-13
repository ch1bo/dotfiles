{ config, pkgs, ... }:

{
  virtualisation.libvirtd = {
    enable = true;

    # TPM for windows 11
    qemu.swtpm.enable = true;
  };

  virtualisation.spiceUSBRedirection.enable = true;

  # Allow VM management
  users.groups.libvirtd.members = [ config.user.name ];
  users.groups.kvm.members = [ config.user.name ];

  # Frontend
  programs.virt-manager.enable = true;

  # Filesystem sharing
  environment.systemPackages = [ pkgs.virtiofsd ];
}
