# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [ "nvme" "ahci" "xhci_pci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelPackages = pkgs.linuxPackages_6_1;
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    # 12GB max ARC cache
    "zfs.zfs_arc_max=12884901888"
    # ASUS mainboards with I225-V seem to have problems with power saving
    # https://www.reddit.com/r/buildapc/comments/xypn1m/network_card_intel_ethernet_controller_i225v_igc/
    "pcie_port_pm=off"
    "pcie_aspm.policy=performance"
  ];
  boot.zfs.allowHibernation = true; # safe because swap is not on zfs

  fileSystems."/" = {
    device = "root/safe/root";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-id/nvme-Samsung_SSD_980_PRO_1TB_S5GXNF0R440049W-part1";
    fsType = "vfat";
  };

  fileSystems."/nix" = {
    device = "root/local/nix";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "root/safe/home";
    fsType = "zfs";
  };

  fileSystems."/home/ch1bo/backup" = {
    device = "data/ch1bo";
    fsType = "zfs";
  };

  swapDevices = [{
    device = "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_1TB_S5H9NS1NB18061Z-part7";
  }];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
