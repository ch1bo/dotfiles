{ config, pkgs, pkgs-unstable, lib, system, inputs, ... }:

{
  services.nginx.virtualHosts."laendlefinder.ncoding.at" =
    {
      forceSSL = true;
      enableACME = true;
      root = inputs.laendlefinder.packages.${system}.default;
      locations."/properties.csv".root = "/data/laendlefinder";
    };
}
