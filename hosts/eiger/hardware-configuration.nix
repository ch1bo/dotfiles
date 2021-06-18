# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  boot.initrd.luks.devices = {
    # ZFS pool: rpool
    root = { device = "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_1TB_S5H9NS1NB18061Z-part2"; };
    # Encrypted swap
    swap = { device = "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_1TB_S5H9NS1NB18061Z-part3"; };
    # ZFS pool: backup
    backup1 = { device = "/dev/disk/by-id/ata-WDC_WD1001FALS-00J7B1_WD-WMATV2660055"; };
    backup2 = { device = "/dev/disk/by-id/ata-WDC_WD1001FALS-00J7B1_WD-WMATV2744001"; };
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_1TB_S5H9NS1NB18061Z-part1";
    fsType = "vfat";
  };

  fileSystems."/" = {
    device = "rpool/safe/root";
    fsType = "zfs";
  };

  fileSystems."/nix" = {
    device = "rpool/local/nix";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "rpool/safe/home";
    fsType = "zfs";
  };

  fileSystems."/home/ch1bo/backup" = {
    device = "backup/ch1bo";
    fsType = "zfs";
  };

  swapDevices = [{ device = "/dev/mapper/swap"; }];

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;
}
