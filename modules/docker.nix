# Docker daemon and command line tool as used on all my hosts.
{
  pkgs,
  ...
}:

{
  # Docker deamon
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";
  environment.systemPackages = with pkgs; [
    docker-buildx # docker buildx
    docker-compose # docker compose
    docker-credential-helpers # store docker login credentials in D-Bus secrets
  ];
}
