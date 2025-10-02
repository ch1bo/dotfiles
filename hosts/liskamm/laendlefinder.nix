{ config, pkgs, lib, system, inputs, ... }:

{
  services.nginx.virtualHosts."laendlefinder.ncoding.at" = {
    forceSSL = true;
    enableACME = true;
    root = inputs.laendlefinder.packages.${system}.default;
    locations."/properties.csv".root = "/data/laendlefinder";
    # NOTE: Deliberately clear text as a very basic access protection is enough
    # to claim this to be non-public
    basicAuth = { user = "fuchsbau"; };
  };
}
