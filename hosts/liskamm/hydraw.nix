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
    inputs.mithril.packages.${system}.mithril-client-cli
  ];

  # The hydraw application / bridge
  virtualisation.oci-containers.containers.hydraw = {
    image = "ghcr.io/input-output-hk/hydraw";
    volumes = [
      "/data/credentials:/credentials:ro"
    ];
    environment = {
      HYDRAW_CARDANO_SIGNING_KEY = "/credentials/wallet.sk";
      HYDRA_API_HOST = "localhost:4002";
      HYDRAW_NETWORK = "2";
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

  virtualisation.oci-containers.containers.cardano-node-preview = {
    image = "ghcr.io/intersectmbo/cardano-node:8.7.3";
    volumes = [
      "/data/cardano-node-preview:/data"
    ];
    cmd = [ "run" ];
    environment = {
      CARDANO_CONFIG = "/data/config/preview_p2p/cardano-node/config.json";
      CARDANO_TOPOLOGY = "/data/config/preview_p2p/cardano-node/topology.json";
      CARDANO_DATABASE_PATH = "/data/db";
      CARDANO_SOCKET_PATH = "/data/node.socket";
      CARDANO_LOG_DIR = "/data/logs";
    };
  };

  virtualisation.oci-containers.containers.hydra-node-preview =
    let
      networkMagic = "2"; # preview
      hydraScriptsTxId = "68011efe6c95d033db263a58612b061868652a084534e5fff890c81a1d94fa85";
      nodeId = "sebastian@preview";
    in
    {
      image = "hydra-node:spike-query";
      volumes = [
        "/data/cardano-node-preview:/cardano-node:ro"
        "/data/credentials:/credentials:ro"
        "/data/hydra-node-preview:/data"
      ];
      ports = [
        "4002:4001"
        "5002:5001"
      ];
      cmd = builtins.concatLists [
        [ "--node-id" nodeId ]
        [ "--api-host" "0.0.0.0" ]
        [ "--host" "0.0.0.0" ]
        [ "--port" "5001" ]
        [ "--monitoring-port" "6001" ]
        [ "--persistence-dir" "/data" ]
        [ "--hydra-scripts-tx-id" hydraScriptsTxId ]
        [ "--hydra-signing-key" "/credentials/sebastian.hydra.sk" ]
        [ "--cardano-signing-key" "/credentials/sebastian.cardano.sk" ]
        [ "--ledger-protocol-parameters" "/data/protocol-parameters.json" ]
        [ "--testnet-magic" networkMagic ]
        [ "--node-socket" "/cardano-node/node.socket" ]
        # [ "--start-chain-from" "35819567.abf6e1083ec8b542173a811ae16b939cc26f8cedeae8123464887512cccca4e0" ]
        [ "--peer" "cardano.hydra.bzh:5001" ] # arnaud
        [ "--cardano-verification-key" "/credentials/arnaud.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/arnaud.hydra.vk" ]
        [ "--peer" "13.37.150.125:5001" ] # sasha
        [ "--cardano-verification-key" "/credentials/sasha.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/sasha.hydra.vk" ]
        [ "--peer" "13.39.44.251:5001" ] # franco
        [ "--cardano-verification-key" "/credentials/franco.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/franco.hydra.vk" ]
        [ "--peer" "hydra.horizon-haskell.net:5005" ] # dan
        [ "--cardano-verification-key" "/credentials/dan.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/dan.hydra.vk" ]
      ];
    };

  ## MAINNET deployment

  virtualisation.oci-containers.containers.cardano-node-mainnet = {
    image = "ghcr.io/intersectmbo/cardano-node:8.9.1";
    volumes = [
      "/data/cardano-node-mainnet:/data"
    ];
    cmd = [ "run" ];
    environment = {
      CARDANO_CONFIG = "/data/config/cardano-node/config.json";
      CARDANO_TOPOLOGY = "/data/config/cardano-node/topology.json";
      CARDANO_DATABASE_PATH = "/data/db";
      CARDANO_SOCKET_PATH = "/data/node.socket";
      CARDANO_LOG_DIR = "/data/logs";
    };
  };

  virtualisation.oci-containers.containers.hydra-node-mainnet =
    let
      hydraScriptsTxId = "2d52e5787b198daeb280f9de63e5dec126b1843b050e85b1642ff8e47cb6de73";
      nodeId = "sebastian@mainnet";
    in
    {
      image = "ghcr.io/input-output-hk/hydra-node:0.16.0";
      volumes = [
        "/data/cardano-node-mainnet:/cardano-node:ro"
        "/data/credentials:/credentials:ro"
        "/data/hydra-node:/data"
      ];
      ports = [
        "4001:4001"
        "5001:5001"
      ];
      cmd = builtins.concatLists [
        [ "--node-id" nodeId ]
        [ "--api-host" "0.0.0.0" ]
        [ "--host" "0.0.0.0" ]
        [ "--port" "5001" ]
        [ "--monitoring-port" "6001" ]
        [ "--persistence-dir" "/data" ]
        [ "--hydra-scripts-tx-id" hydraScriptsTxId ]
        [ "--hydra-signing-key" "/credentials/sebastian.hydra.sk" ]
        [ "--cardano-signing-key" "/credentials/sebastian.cardano.sk" ]
        [ "--ledger-protocol-parameters" "/data/protocol-parameters.json" ]
        [ "--mainnet" ]
        [ "--node-socket" "/cardano-node/node.socket" ]
        # [ "--start-chain-from" "92679263.9a7bcacdf4c862e4df776ad54eca51dbd4bf1a8ee036d9d10d41f81e84020028" ]
        # [ "--peer" "cardano.hydra.bzh:5001" ] # arnaud
        # [ "--cardano-verification-key" "/credentials/arnaud.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/arnaud.hydra.vk" ]
        # [ "--peer" "13.37.150.125:5001" ] # sasha
        # [ "--cardano-verification-key" "/credentials/sasha.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/sasha.hydra.vk" ]
        # [ "--peer" "13.39.83.131:5001" ] # franco
        # [ "--cardano-verification-key" "/credentials/franco.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/franco.hydra.vk" ]
        # [ "--peer" "hydra.horizon-haskell.net:5005" ] # dan
        # [ "--cardano-verification-key" "/credentials/dan.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/dan.hydra.vk" ]
      ];
    };

  # Log aggregation
  # services.grafana-agent = {
  #   enable = true;
  #   settings = builtins.fromJSON (builtins.readFile ./grafana-agent.json);
  #   credentials = {
  #     GRAFANA_API_KEY = "/run/keys/grafana/api-key";
  #   };
  # };
}
