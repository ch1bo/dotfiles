# Hydraw running on hydra running on cardano

{ config, pkgs, lib, ... }:

{
  # services.nginx.virtualHosts."hydraw.fk.ncoding.at" = {
  #   forceSSL = true;
  #   enableACME = true;
  #   locations."/" = {
  #     proxyPass = "http://127.0.0.1:2342";
  #     proxyWebsockets = true;
  #     extraConfig = ''
  #       proxy_buffering off;
  #       client_max_body_size 500M;
  #     '';
  #   };
  # };

  # using entrypoint from: https://github.com/input-output-hk/cardano-world/blob/master/nix/cardano/entrypoints.nix
  # example invocation:
  # podman run -d
  #   -v data:/data
  #   -e DATA_DIR=/data
  #   -e ENVIRONMENT=testnet
  #   -e SOCKET_PATH=/data/node.socket
  #   -e USE_SNAPSHOT=1
  #   inputoutput/1.35.3-new
  virtualisation.oci-containers.containers.cardano-node = {
    image = "inputoutput/cardano-node:1.35.3-new";
    volumes = [
      "/data/cardano-node:/data"
    ];
    environment = {
      DATA_DIR = "/data";
      ENVIRONMENT = "preview";
      SOCKET_PATH = "/data/node.socket";
      USE_SNAPSHOT = "1";
    };
  };
}
