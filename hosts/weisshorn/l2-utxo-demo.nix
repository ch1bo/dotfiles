# A NixOS configuration for the L2 UTXO demo
#
# This includes
# - a user account for txpipe
# - a preprod cadano-node
# - docker compose to run backend services
# - a reverse proxy to expose the frontend
{
  users.users.txpipe = {
    isNormalUser = true;
    home = "/home/txpipe";
    extraGroups = [ "docker" ];
    openssh.authorizedKeys.keys = [
      (builtins.readFile ./ignacio.pub)
      (builtins.readFile ./valentino.pub)
    ];
  };

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
  services.nginx.enable = true;
  services.nginx.recommendedProxySettings = true;
  security.acme.acceptTerms = true;
  security.acme.defaults.email = "webmaster@ncoding.li";
  services.nginx.virtualHosts."l2-eutxo-interop.cardano-scaling.org" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      return = "200 '<html><body>It works</body></html>'";
      extraConfig = ''
        default_type text/html;
      '';
    };
  };

  virtualisation.docker.enable = true;
  virtualisation.oci-containers.backend = "docker";
  virtualisation.oci-containers.containers.cardano-node-preprod = {
    image = "ghcr.io/intersectmbo/cardano-node:10.6.2";
    volumes = [
      "/data/cardano-configurations/network/preprod:/config"
      "/data/cardano-node-preprod:/data"
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
}
