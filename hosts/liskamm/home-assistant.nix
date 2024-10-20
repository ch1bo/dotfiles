# Home assistant as container
#
# https://www.home-assistant.io/installation/linux#install-home-assistant-container
# https://nixos.wiki/wiki/Home_Assistant#OCI_container
{ config, pkgs, lib, inputs, system, ... }:
let
  port = 8123;
in
{
  # XXX: really expose port of only proxy it?
  networking.firewall.allowedTCPPorts = [ port ];

  services.nginx.virtualHosts."home.ncoding.at" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${builtins.toString port}";
      proxyWebsockets = true;
      # XXX: check proxy settings; why not recommended settings?
      recommendedProxySettings = false;
    };
  };

  virtualisation.oci-containers.containers.home-assistant = {
    image = "ghcr.io/home-assistant/home-assistant:2024.5.0b1";
    volumes = [
      # TODO: Backup config + databases
      "/data/home-assistant:/config"
    ];
    environment.TZ = "Europe/Vienna";
    extraOptions = [
      "--network=host"
      "--device=/dev/ttyUSB0:/dev/ttyUSB0"
    ];
  };
}
