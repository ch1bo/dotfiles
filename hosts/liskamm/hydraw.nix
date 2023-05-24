# Hydraw running on hydra running on cardano

{ config, pkgs, lib, inputs, system, ... }:

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

  # virtualisation.oci-containers.containers.cardano-node = {
  #   image = "inputoutput/cardano-node:1.35.5";
  #   volumes = [
  #     "/data/cardano-node-preprod:/data"
  #   ];
  #   cmd = [ "run" ];
  #   environment = {
  #     CARDANO_CONFIG = "/data/config/preprod/cardano-node/config.json";
  #     CARDANO_TOPOLOGY = "/data/config/preprod/cardano-node/topology.json";
  #     CARDANO_DATABASE_PATH = "/data/db";
  #     CARDANO_SOCKET_PATH = "/data/node.socket";
  #     CARDANO_LOG_DIR = "/data/logs";
  #   };
  # };

  virtualisation.oci-containers.containers.cardano-node-mainnet = {
    image = "inputoutput/cardano-node:1.35.5";
    volumes = [
      "/data/cardano-node-mainnet:/data"
    ];
    cmd = [ "run" ];
    environment = {
      CARDANO_CONFIG = "/data/config/mainnet/cardano-node/config.json";
      CARDANO_TOPOLOGY = "/data/config/mainnet/cardano-node/topology.json";
      CARDANO_DATABASE_PATH = "/data/db";
      CARDANO_SOCKET_PATH = "/data/node.socket";
      CARDANO_LOG_DIR = "/data/logs";
    };
  };

  # Let's add the command line tools directly for more convenience
  environment.systemPackages = [
    inputs.cardano-node.packages.${system}.cardano-cli
    inputs.hydra.packages.${system}.hydra-tui-static
    inputs.hydra.packages.${system}.hydra-tools-static
  ];

  # Our hydra-node instance
  virtualisation.oci-containers.containers.hydra-node =
    # TODO: lookup by network (mainnet)
    let
      hydraScriptsTxId = "09285781ab2ae4873b27a0853fdbddb0c1e06f2eaf4fb31812d4439efdf48a2c";
      nodeId = "sebastian-node";
    in
    {
      image = "ghcr.io/input-output-hk/hydra-node@sha256:c7279461f6a08ac99620c912c102845919612e1d156ec58e89b501532a731a73";
      volumes = [
        "/data/cardano-node-mainnet:/cardano-node:ro"
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
        [ "--persistence-dir" "/data" ]
        [ "--hydra-scripts-tx-id" hydraScriptsTxId ]
        [ "--hydra-signing-key" "/credentials/sebastian.hydra.sk" ]
        [ "--cardano-signing-key" "/credentials/sebastian.cardano.sk" ]
        [ "--ledger-genesis" "/cardano-node/config/mainnet/genesis/shelley.json" ]
        [ "--ledger-protocol-parameters" "/data/protocol-parameters.json" ]
        [ "--mainnet" ]
        [ "--node-socket" "/cardano-node/node.socket" ]
        # [ "--start-chain-from" "87849765.9a1756f043d0dca3cd5b217b7b162582644e5989d815f81941156a62dd70795d" ]
        [ "--peer" "cardano.hydra.bzh:5001" ] # arnaud
        [ "--cardano-verification-key" "/credentials/arnaud.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/arnaud.hydra.vk" ]
        # [ "--peer" "13.37.15.211:5001" ] # pascal
        # [ "--cardano-verification-key" "/credentials/pascal.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/pascal.hydra.vk" ]
        [ "--peer" "13.37.150.125:5001" ] # sasha
        [ "--cardano-verification-key" "/credentials/sasha.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/sasha.hydra.vk" ]
        [ "--peer" "13.38.189.209:5001" ] # franco
        [ "--cardano-verification-key" "/credentials/franco.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/franco.hydra.vk" ]
      ];
    };

  # The hydraw application / bridge
  virtualisation.oci-containers.containers.hydraw = {
    image = "ghcr.io/input-output-hk/hydraw";
    volumes = [
      "/data/credentials:/credentials:ro"
    ];
    environment = {
      HYDRAW_CARDANO_SIGNING_KEY = "/credentials/sebastian.cardano.sk";
      HYDRA_API_HOST = "localhost:4001";
      HYDRAW_NETWORK = "mainnet";
    };
    extraOptions = [ "--network=host" ];
  };

  # Configure the reverse proxy to point at it
  services.nginx.virtualHosts."hydraw.ncoding.at" = {
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

  # Log aggregation
  services.grafana-agent = {
    enable = true;
    settings = builtins.fromJSON (builtins.readFile ./grafana-agent.json);
    credentials = {
      GRAFANA_API_KEY = "/run/keys/grafana/api-key";
    };
  };
}
