# Local mithril aggregator mock HTTP endpoint
#
# Serving the following files from /srv (for the record):
# /srv/mithril-aggregator-root
# /srv/mithril-aggregator-root/aggregator
# /srv/mithril-aggregator-root/aggregator/artifact
# /srv/mithril-aggregator-root/aggregator/artifact/snapshot
# /srv/mithril-aggregator-root/aggregator/artifact/snapshot/ed8c5d5d0cd96d0e4ac154409486447aed14c20de3b5b9bb6baa487b2fb0a84a
# /srv/mithril-aggregator-root/aggregator/artifact/snapshot/f86298dc4e0fda1a75f6f6ad7a8031554845f6d33364d9e08bb866f179a15756
# /srv/mithril-aggregator-root/aggregator/artifact/snapshot/f1bfa639e68308e3ee6ed8fc9b3ee1063a35a99a98f3c74fe907cad9a4d84ebb
# /srv/mithril-aggregator-root/aggregator/preprod-e90-i1727.f86298dc4e0fda1a75f6f6ad7a8031554845f6d33364d9e08bb866f179a15756.tar.gz
# /srv/mithril-aggregator-root/aggregator/preprod-e90-i1726.f1bfa639e68308e3ee6ed8fc9b3ee1063a35a99a98f3c74fe907cad9a4d84ebb.tar.gz
# /srv/mithril-aggregator-root/x86_64-darwin
# /srv/mithril-aggregator-root/x86_64-darwin/hydra-node

{ config, pkgs, lib, inputs, system, ... }:

{

  networking.firewall.allowedTCPPorts = [ 80 ];
  services.nginx = {
    enable = true;
    virtualHosts."mithril-aggregator" = {
      root = "/srv/mithril-aggregator-root";
      locations = {
        "/" = {
          tryFiles = "$uri $uri/ @backend";
        };
        "@backend" = {
          proxyPass = "https://aggregator.release-preprod.api.mithril.network";
        };
      };
    };
  };
}
