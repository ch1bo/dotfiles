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

  # Let's add the command line tools directly for more convenience
  environment.systemPackages = [
    inputs.cardano-node.packages.${system}.cardano-cli
    inputs.hydra.packages.${system}.hydra-tui-static
  ];

  # The hydraw application / bridge
  virtualisation.oci-containers.containers.hydraw = {
    image = "ghcr.io/input-output-hk/hydraw";
    volumes = [
      "/data/credentials:/credentials:ro"
    ];
    environment = {
      HYDRAW_CARDANO_SIGNING_KEY = "/credentials/wallet.sk";
      HYDRA_API_HOST = "localhost:4001";
      HYDRAW_NETWORK = "1";
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

  ## Testnet deployment

  # virtualisation.oci-containers.containers.cardano-node-preprod = {
  #   image = "inputoutput/cardano-node:1.35.7";
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

  # virtualisation.oci-containers.containers.hydra-node-preprod =
  #   let
  #     networkMagic = "1"; # preprod
  #     hydraScriptsTxId = "fad1d233c37d64f3276dcebd6d10d4ecc8935c504e73d78a7285acc22ff534d4";
  #     nodeId = "sebastian@preprod";
  #   in
  #   {
  #     image = "ghcr.io/input-output-hk/hydra-node@sha256:3b7f78962ff8fc212644e1ef4faf8742ead6f7c31353cdf5cc251d4d825edac7";
  #     volumes = [
  #       "/data/cardano-node-preprod:/cardano-node:ro"
  #       "/data/credentials:/credentials:ro"
  #       "/data/hydra-node-preprod:/data"
  #     ];
  #     ports = [
  #       "4101:4001"
  #       "5101:5003"
  #     ];
  #     cmd = builtins.concatLists [
  #       [ "--node-id" nodeId ]
  #       [ "--api-host" "0.0.0.0" ]
  #       [ "--host" "0.0.0.0" ]
  #       [ "--port" "5003" ]
  #       [ "--monitoring-port" "6001" ]
  #       [ "--persistence-dir" "/data" ]
  #       [ "--hydra-scripts-tx-id" hydraScriptsTxId ]
  #       [ "--hydra-signing-key" "/credentials/sebastian.hydra.sk" ]
  #       [ "--cardano-signing-key" "/credentials/sebastian.cardano.sk" ]
  #       [ "--ledger-protocol-parameters" "/data/protocol-parameters.json" ]
  #       [ "--testnet-magic" networkMagic ]
  #       [ "--node-socket" "/cardano-node/node.socket" ]
  #       [ "--start-chain-from" "35819567.abf6e1083ec8b542173a811ae16b939cc26f8cedeae8123464887512cccca4e0" ]
  #       # [ "--peer" "www.punkachien.net:5001" ] # arnaud
  #       # [ "--cardano-verification-key" "/credentials/arnaud.cardano.vk" ]
  #       # [ "--hydra-verification-key" "/credentials/arnaud.hydra.vk" ]
  #       # [ "--peer" "13.37.15.211:5001" ] # pascal
  #       # [ "--cardano-verification-key" "/credentials/pascal.cardano.vk" ]
  #       # [ "--hydra-verification-key" "/credentials/pascal.hydra.vk" ]
  #       [ "--peer" "13.37.150.125:5001" ] # sasha
  #       [ "--cardano-verification-key" "/credentials/sasha.cardano.vk" ]
  #       [ "--hydra-verification-key" "/credentials/sasha.hydra.vk" ]
  #       [ "--peer" "13.39.148.175:5001" ] # franco
  #       [ "--cardano-verification-key" "/credentials/franco.cardano.vk" ]
  #       [ "--hydra-verification-key" "/credentials/franco.hydra.vk" ]
  #       [ "--peer" "hydra.horizon-haskell.net:5005" ] # daniel
  #       [ "--cardano-verification-key" "/credentials/daniel.cardano.vk" ]
  #       [ "--hydra-verification-key" "/credentials/daniel.hydra.vk" ]
  #     ];
  #   };

  ## MAINNET deployment

  virtualisation.oci-containers.containers.cardano-node-mainnet = {
    image = "inputoutput/cardano-node:8.1.2";
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

  # Our hydra-node instance
  virtualisation.oci-containers.containers.hydra-node =
    # TODO: lookup by network (mainnet)
    let
      hydraScriptsTxId = "8de073d8429b16684b2e6b76371462fdedc07bbecfcb32c810fee1194c350e34";
      nodeId = "sebastian-node";
    in
    {
      image = "ghcr.io/input-output-hk/hydra-node@sha256:4639701faae382d8be44a832d3196d1320c26203a196aa708f337e8cfdd5ea88";
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
        [ "--ledger-protocol-parameters" "/data/protocol-parameters.json" ]
        [ "--mainnet" ]
        [ "--node-socket" "/cardano-node/node.socket" ]
        # [ "--start-chain-from" "92679263.9a7bcacdf4c862e4df776ad54eca51dbd4bf1a8ee036d9d10d41f81e84020028" ]
        [ "--peer" "cardano.hydra.bzh:5001" ] # arnaud
        [ "--cardano-verification-key" "/credentials/arnaud.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/arnaud.hydra.vk" ]
        [ "--peer" "13.37.150.125:5001" ] # sasha
        [ "--cardano-verification-key" "/credentials/sasha.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/sasha.hydra.vk" ]
        [ "--peer" "35.181.45.240:5001" ] # franco
        [ "--cardano-verification-key" "/credentials/franco.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/franco.hydra.vk" ]
        [ "--peer" "hydra.horizon-haskell.net:5005" ] # dan
        [ "--cardano-verification-key" "/credentials/daniel.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/daniel.hydra.vk" ]
      ];
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
