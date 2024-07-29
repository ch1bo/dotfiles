# Hydraw running on hydra running on cardano

{ config, pkgs, lib, inputs, system, ... }:

{
  # Add iohk substituters
  nix.settings.substituters = [
    "https://cache.nixos.org"
    "https://cache.iog.io"
    "https://cardano-scaling.cachix.org"
  ];
  nix.settings.trusted-public-keys = [
    "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY="
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
    image = "ghcr.io/intersectmbo/cardano-node:9.1.0";
    volumes = [
      "/data/cardano-configurations/network/preview:/config"
      "/data/cardano-node-preview:/data"
    ];
    cmd = [ "run" ];
    environment = {
      CARDANO_CONFIG = "/config/cardano-node/config.json";
      CARDANO_TOPOLOGY = "/config/cardano-node/topology.json";
      CARDANO_DATABASE_PATH = "/data/db";
      CARDANO_SOCKET_PATH = "/data/node.socket";
      CARDANO_LOG_DIR = "/data/logs";
    };
  };

  virtualisation.oci-containers.containers.hydra-node-preview =
    let
      networkMagic = "2"; # preview
      hydraScriptsTxId = "18db4e6639c864250e5b0e6ef952d5262cb5873ae9268208c9b9411414c658a8";
      nodeId = "sebastian@preview";
    in
    {
      image = "ghcr.io/cardano-scaling/hydra-node@sha256:c667730f306d3f4eab8b2a1c149d0ceae8674ff61e2ff23a482646cfc531ec3e";
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
        [ "--start-chain-from" "54646384.1d7ce81b5b2ba42f83adf39db7755819a419a3883d1bf984b8b5b9c88956b954" ]
        # [ "--peer" "cardano.hydra.bzh:5001" ] # arnaud
        # [ "--cardano-verification-key" "/credentials/arnaud.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/arnaud.hydra.vk" ]
        [ "--peer" "13.37.150.125:5001" ] # sasha
        [ "--cardano-verification-key" "/credentials/sasha.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/sasha.hydra.vk" ]
        [ "--peer" "13.39.44.251:5001" ] # franco
        [ "--cardano-verification-key" "/credentials/franco.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/franco.hydra.vk" ]
        [ "--peer" "hydra.horizon-haskell.net:5005" ] # dan
        [ "--cardano-verification-key" "/credentials/dan.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/dan.hydra.vk" ]
        # [ "--peer" "87.212.22.225:5551" ] # reza
        # [ "--cardano-verification-key" "/credentials/reza.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/reza.hydra.vk" ]
        [ "--peer" "35.214.9.104:5005" ] # noon
        [ "--cardano-verification-key" "/credentials/noon.cardano.vk" ]
        [ "--hydra-verification-key" "/credentials/noon.hydra.vk" ]
      ];
    };

  ## MAINNET deployment

  virtualisation.oci-containers.containers.cardano-node-mainnet = {
    image = "ghcr.io/intersectmbo/cardano-node:9.1.0";
    volumes = [
      "/data/cardano-configurations/network/mainnet:/config"
      "/data/cardano-node-mainnet:/data"
    ];
    cmd = [ "run" ];
    environment = {
      CARDANO_CONFIG = "/config/cardano-node/config.json";
      CARDANO_TOPOLOGY = "/config/cardano-node/topology.json";
      CARDANO_DATABASE_PATH = "/data/db";
      CARDANO_SOCKET_PATH = "/data/node.socket";
      CARDANO_LOG_DIR = "/data/logs";
    };
  };

  virtualisation.oci-containers.containers.hydra-node-mainnet =
    let
      hydraScriptsTxId = "0d2eca8c8daf23061f5ba24a8f3113afba52a2e26318dc78c5583d9fc86b7b85";
      nodeId = "sebastian@mainnet";
    in
    {
      image = "ghcr.io/input-output-hk/hydra-node:0.17.0";
      volumes = [
        "/data/cardano-node-mainnet:/cardano-node:ro"
        "/data/credentials:/credentials:ro"
        "/data/hydra-node-mainnet:/data"
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
