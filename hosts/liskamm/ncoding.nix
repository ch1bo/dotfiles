{
  config,
  pkgs,
  lib,
  system,
  inputs,
  ...
}:

{
  options.ncoding.publicInterface = lib.mkOption {
    type = lib.types.str;
    description = ''
      Network interface on which liskamm's web services should be reachable.
      The reverse proxy on weisshorn is the only expected client, so this is
      normally the wireguard tunnel to it; plain HTTP is not exposed on any
      other interface.
    '';
  };

  config = {
    networking.firewall.interfaces.${config.ncoding.publicInterface}.allowedTCPPorts = [ 80 ];

    services.nginx.enable = true;
    services.nginx.recommendedProxySettings = true;
    services.nginx.virtualHosts."www.ncoding.at" =
      let
        ncoding-web = pkgs.symlinkJoin {
          name = "ncoding.at";
          paths = [
            ./ncoding.at
            inputs.cv.packages.${system}.cv
          ];
        };
      in
      {
        serverAliases = [
          "ncoding.at"
          "ncoding.li"
          "www.ncoding.li"
        ];
        root = ncoding-web;
      };
  };
}
