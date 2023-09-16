# Local mithril aggregator mock HTTP endpoint

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
