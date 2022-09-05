# Hydraw running on hydra running on cardano

{ config, pkgs, lib, ... }:

let
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "1.35.3";
      sha256 = "020fwimsm24yblr1fmnwx240wj8r3x715p89cpjgnnd8axwf32p0";
    })
    { };
in
{
  # Add iohk substituters
  nix.settings.substituters = [
    "https://cache.nixos.org"
    "https://hydra.iohk.io"
  ];
  nix.settings.trusted-public-keys = [
    "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
  ];

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
  virtualisation.oci-containers.containers.cardano-node = {
    image = "inputoutput/cardano-node:1.35.3-new";
    volumes = [
      "/data/cardano-node:/data"
    ];
    environment = {
      DATA_DIR = "/data";
      ENVIRONMENT = "preview";
      SOCKET_PATH = "/data/node.socket";
    };
  };

  # The 1.35.3-new cardano-node image does not contain a cli, so let's add it
  # using nix instead.
  environment.systemPackages = [ cardano-node.cardano-cli ];
}
