# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "usbhid" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "rpool/safe/root";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    { device = "rpool/local/nix";
      fsType = "zfs";
    };

  fileSystems."/home" =
    { device = "rpool/safe/home";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/7A46-F964";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/36a85a00-69b8-438d-9c7d-62ad3c68ad68"; }
    ];

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
