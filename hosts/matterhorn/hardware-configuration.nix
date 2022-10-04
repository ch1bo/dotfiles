# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  # TODO: override should not be necessary anymore in unstable/22.11
  boot.kernelPackages = pkgs.linuxPackages_5_19.extend (final: prev: {
    zfs = prev.zfs.overrideAttrs ({ NIX_CFLAGS_COMPILE ? [], ... }: {
      NIX_CFLAGS_COMPILE = NIX_CFLAGS_COMPILE ++ [ "-Wno-error=attribute-warning" ];
    });
  });
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "root/safe/root";
      fsType = "zfs";
    };

  fileSystems."/home" =
    { device = "root/safe/home";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    { device = "root/local/nix";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/BB1A-3903";
      fsType = "vfat";
    };

  swapDevices = [ { device = "/dev/disk/by-id/nvme-WDC_PC_SN730_SDBQNTY-512G-1001_21514Q802807-part3"; } ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
