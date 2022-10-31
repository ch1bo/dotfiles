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

  hydraSrc = pkgs.fetchgit {
    url = "https://github.com/input-output-hk/hydra-poc.git";
    rev = "af88801e1395b0e0194fd63e20fbc79597a78269";
    sha256 = "0mw5l4hbwrsw2fcjb95ibdijh88icgp30ldbi1hgpn5snd5kizzb";
  };
  hydra = import (hydraSrc.outPath + "/release.nix") { };
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

  # Using entrypoint from: https://github.com/input-output-hk/cardano-world/blob/master/nix/cardano/entrypoints.nix
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
  environment.systemPackages = [
    cardano-node.cardano-cli
    hydra.hydra-tui-static
    hydra.hydra-tools-static
  ];

  # Our hydra-node instance
  virtualisation.oci-containers.containers.hydra-node =
    # TODO: lookup by network (preview)
    let
      hydraScriptsTxId = "bde2ca1f404200e78202ec37979174df9941e96fd35c05b3680d79465853a246";
      networkMagic = "2";
      nodeId = "sebastian's node";
    in
    {
      image = "ghcr.io/input-output-hk/hydra-node:0.8.0";
      volumes = [
        "/data/cardano-node:/cardano-node:ro"
        "/data/credentials:/credentials:ro"
        "/data/hydra-node:/data"
      ];
      ports = [
        "4001:4001"
        "5001:5003"
      ];
      cmd = builtins.concatLists [
        [ "--node-id" nodeId ]
        [ "--api-host" "0.0.0.0" ]
        [ "--host" "0.0.0.0" ]
        [ "--port" "5003" ]
        [ "--monitoring-port" "6001" ]
        [ "--hydra-scripts-tx-id" hydraScriptsTxId ]
        [ "--hydra-signing-key" "/credentials/sebastian.hydra.sk" ]
        [ "--cardano-signing-key" "/credentials/sebastian.cardano.sk" ]
        [ "--ledger-genesis" "/cardano-node/config/preview/shelley-genesis.json" ]
        [ "--ledger-protocol-parameters" "/data/protocol-parameters.json" ]
        [ "--network-id" networkMagic ]
        [ "--node-socket" "/cardano-node/node.socket" ]
        # [ "--start-chain-from" "4268491.c628e6fb4e697500aa338c82d80c6b6c1e91710589f98eee2004ec2879ef3f98" ]
        # [ "--peer" "35.233.17.169:5001" ] # arnaud
        # [ "--cardano-verification-key" "/credentials/arnaud.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/arnaud.hydra.vk" ]
        [ "--peer" "13.39.80.222:5001" ] # pascal
        [ "--cardano-verification-key" "/credentials/pascal.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/pascal.hydra.vk" ]
        [ "--peer" "13.38.49.252:5001" ] # sasha
        [ "--cardano-verification-key" "/credentials/sasha.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/sasha.hydra.vk" ]
        [ "--peer" "13.38.189.209:5001" ] # franco
        [ "--cardano-verification-key" "/credentials/franco.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/franco.hydra.vk" ]
      ];
    };

  # The hydraw application / bridge
  virtualisation.oci-containers.containers.hydraw = {
    image = "ghcr.io/input-output-hk/hydraw:latest";
    volumes = [
      "/data/credentials:/credentials:ro"
    ];
    entrypoint = "hydraw";
    environment = {
      HYDRAW_CARDANO_SIGNING_KEY = "/credentials/sebastian.cardano.sk";
      HYDRA_API_HOST = "localhost:4001";
    };
    extraOptions = [ "--network=host" ];
  };

  # Configure the reverse proxy to point at it
  services.nginx.virtualHosts."hydraw.fk.ncoding.at" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:1337";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 500M;
      '';
    };
  };
}
