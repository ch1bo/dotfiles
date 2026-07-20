# Docker daemon and command line tool as used on all my hosts.
{
  pkgs,
  ...
}:

{
  virtualisation.docker = {
    # Docker deamon
    enable = true;
    # overlayfs is faster than zfs, is compatible with zfs now, and zfs driver was
    # create so many datasets/snapshots
    storageDriver = "overlay2";
    # Deliberately pick latest because default (docker_28) is insecure
    package = pkgs.docker_29;
  };
  environment.systemPackages = with pkgs; [
    docker-credential-helpers # store docker login credentials in D-Bus secrets
  ];
}
