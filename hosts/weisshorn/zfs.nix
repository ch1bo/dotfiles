# See README.org for full disk setup
{
  # Enable zfs support
  boot.supportedFilesystems = [ "zfs" ];

  # 12GB max ARC cache
  boot.kernelParams = [ "zfs.zfs_arc_max=12884901888" ];

  # Import data even though it's not declaratively mounted by nix
  boot.zfs.extraPools = [ "data" ];

  # Auto-snapshot all marked datasets, e.g
  # zfs set com.sun:auto-snapshot=true data
  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoSnapshot.flags = "-k -p --utc";
}
