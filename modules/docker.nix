# Docker daemon and command line tool as used on all my hosts.
{
  pkgs,
  ...
}:

{
  # Docker deamon
  virtualisation.docker.enable = true;
  # overlayfs is faster than zfs, is compatible with zfs now, and zfs driver was
  # create so many datasets/snapshots
  virtualisation.docker.storageDriver = "overlay2";
  environment.systemPackages = with pkgs; [
    docker-credential-helpers # store docker login credentials in D-Bus secrets
  ];
}
