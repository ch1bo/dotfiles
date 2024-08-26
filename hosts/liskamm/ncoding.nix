{ config, pkgs, pkgs-unstable, lib, system, inputs, ... }:

{
  security.acme.acceptTerms = true;
  security.acme.defaults.email = "webmaster@ncoding.at";

  services.nginx.enable = true;
  services.nginx.recommendedProxySettings = true;
  services.nginx.virtualHosts."www.ncoding.at" =
    let
      ncoding-web = pkgs.symlinkJoin {
        name = "ncoding.at";
        paths = [ ./ncoding.at inputs.cv.packages.${system}.cv ];
      };
    in
    {
      serverAliases = [ "ncoding.at" ];
      forceSSL = true;
      enableACME = true;
      root = ncoding-web;
    };
}
