# Home assistant as container
#
# https://www.home-assistant.io/installation/linux#install-home-assistant-container
# https://nixos.wiki/wiki/Home_Assistant#OCI_container
{ config, pkgs, lib, inputs, system, ... }:
{
  networking.firewall.allowedTCPPorts = [ 8123 ];
  virtualisation.oci-containers.containers.home-assistant = {
    image = "ghcr.io/home-assistant/home-assistant:stable";
    volumes = [
      # TODO: Backup config + databases
      "/data/home-assistant:/config"
    ];
    environment.TZ = "Europe/Berlin";
    extraOptions = [
      "--network=host"
      "--device=/dev/ttyUSB0:/dev/ttyUSB0"
    ];
  };
}
