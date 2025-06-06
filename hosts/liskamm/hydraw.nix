# Hydraw running on hydra running on cardano

{ config, pkgs, lib, inputs, system, ... }:

{
  # Add iohk substituters
  nix.settings.substituters = [
    "https://cache.iog.io"
    "https://cardano-scaling.cachix.org"
  ];
  nix.settings.trusted-public-keys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "cardano-scaling.cachix.org-1:QNK4nFrowZ/aIJMCBsE35m+O70fV6eewsBNdQnCSMKA="
  ];

  # Let's add the command line tools directly for more convenience
  environment.systemPackages = [
    inputs.cardano-node.packages.${system}.cardano-cli
    inputs.hydra.packages.${system}.hydra-tui-static
    inputs.mithril.packages.${system}.mithril-client-cli
  ];

  # The hydraw application / bridge
  virtualisation.oci-containers.containers.hydraw = {
    image = "ghcr.io/cardano-scaling/hydraw";
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
    image = "ghcr.io/intersectmbo/cardano-node:10.1.3";
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
      hydraScriptsTxId = "b7b88533de303beefae2d8bb93fe1a1cd5e4fa3c4439c8198c83addfe79ecbdc,da1cc0eef366031e96323b6620f57bc166cf743c74ce76b6c3a02c8f634a7d20,6665f1dfdf9b9eb72a0dd6bb73e9e15567e188132b011e7cf6914c39907ac484";
      nodeId = "sebastian@preview";
    in
    {
      image = "ghcr.io/cardano-scaling/hydra-node:unstable";
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
        [ "--listen" "0.0.0.0:5001" ]
        [ "--advertise" "ncoding.at:5001" ]
        [ "--monitoring-port" "6001" ]
        [ "--persistence-dir" "/data" ]
        [ "--hydra-scripts-tx-id" hydraScriptsTxId ]
        [ "--hydra-signing-key" "/credentials/sebastian.hydra.sk" ]
        [ "--cardano-signing-key" "/credentials/sebastian.cardano.sk" ]
        [ "--ledger-protocol-parameters" "/data/protocol-parameters.json" ]
        [ "--testnet-magic" networkMagic ]
        [ "--node-socket" "/cardano-node/node.socket" ]
        # [ "--start-chain-from" "56465662.f257db514ed2900a6502998befb45977f207b05c76defb4a3835fb5098742b9c" ]
        # [ "--peer" "cardano.hydra.bzh:5001" ] # arnaud
        # [ "--cardano-verification-key" "/credentials/arnaud.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/arnaud.hydra.vk" ]
        # [ "--peer" "13.37.150.125:5001" ] # sasha
        # [ "--cardano-verification-key" "/credentials/sasha.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/sasha.hydra.vk" ]
        # [ "--peer" "13.39.44.251:5001" ] # franco
        # [ "--cardano-verification-key" "/credentials/franco.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/franco.hydra.vk" ]
        # [ "--peer" "hydra.horizon-haskell.net:5005" ] # dan
        # [ "--cardano-verification-key" "/credentials/dan.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/dan.hydra.vk" ]
        # [ "--peer" "87.212.22.225:5551" ] # reza
        # [ "--cardano-verification-key" "/credentials/reza.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/reza.hydra.vk" ]
        # [ "--peer" "35.214.9.104:5005" ] # noon
        # [ "--cardano-verification-key" "/credentials/noon.cardano.vk" ]
        # [ "--hydra-verification-key" "/credentials/noon.hydra.vk" ]
      ];
    };

  ## MAINNET deployment

  virtualisation.oci-containers.containers.cardano-node-mainnet = {
    image = "ghcr.io/intersectmbo/cardano-node:10.4.1";
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

  # virtualisation.oci-containers.containers.hydra-node-mainnet =
  #   let
  #     hydraScriptsTxId = "ab1d9f8cca896bca06b70df74860deecf20774e03d8562aecaed37525f6ebead";
  #     nodeId = "sebastian@mainnet";
  #   in
  #   {
  #     image = "ghcr.io/cardano-scaling/hydra-node:0.19.0";
  #     volumes = [
  #       "/data/cardano-node-mainnet:/cardano-node:ro"
  #       "/data/credentials:/credentials:ro"
  #       "/data/hydra-node-mainnet:/data"
  #     ];
  #     ports = [
  #       "4001:4001"
  #       "5001:5001"
  #     ];
  #     cmd = builtins.concatLists [
  #       [ "--node-id" nodeId ]
  #       [ "--api-host" "0.0.0.0" ]
  #       [ "--host" "0.0.0.0" ]
  #       [ "--port" "5001" ]
  #       [ "--monitoring-port" "6001" ]
  #       [ "--persistence-dir" "/data" ]
  #       [ "--hydra-scripts-tx-id" hydraScriptsTxId ]
  #       [ "--hydra-signing-key" "/credentials/sebastian.hydra.sk" ]
  #       [ "--cardano-signing-key" "/credentials/sebastian.cardano.sk" ]
  #       [ "--ledger-protocol-parameters" "/data/protocol-parameters.json" ]
  #       [ "--mainnet" ]
  #       [ "--node-socket" "/cardano-node/node.socket" ]
  #       # [ "--start-chain-from" "131547227.8a3dbf7df13abbf2a840d07223785f59283aed800e3aea70ee9d726f59bb825a" ]
  #       # [ "--peer" "cardano.hydra.bzh:5001" ] # arnaud
  #       # [ "--cardano-verification-key" "/credentials/arnaud.cardano.vk" ]
  #       # [ "--hydra-verification-key" "/credentials/arnaud.hydra.vk" ]
  #       # [ "--peer" "13.37.150.125:5001" ] # sasha
  #       # [ "--cardano-verification-key" "/credentials/sasha.cardano.vk" ]
  #       # [ "--hydra-verification-key" "/credentials/sasha.hydra.vk" ]
  #       # [ "--peer" "13.39.83.131:5001" ] # franco
  #       # [ "--cardano-verification-key" "/credentials/franco.cardano.vk" ]
  #       # [ "--hydra-verification-key" "/credentials/franco.hydra.vk" ]
  #       # [ "--peer" "hydra.horizon-haskell.net:5005" ] # dan
  #       # [ "--cardano-verification-key" "/credentials/dan.cardano.vk" ]
  #       # [ "--hydra-verification-key" "/credentials/dan.hydra.vk" ]
  #     ];
  #   };

  # Log aggregation
  # services.grafana-agent = {
  #   enable = true;
  #   settings = builtins.fromJSON (builtins.readFile ./grafana-agent.json);
  #   credentials = {
  #     GRAFANA_API_KEY = "/run/keys/grafana/api-key";
  #   };
  # };

  # Open hydra-node ports
  networking.firewall.allowedTCPPorts = [ 5001 5002 ];
}
