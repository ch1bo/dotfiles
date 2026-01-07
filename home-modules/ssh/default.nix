{ ... }:
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "eiger" = {
        # hostname = "ncoding.at";
        # port = 2201;
        forwardAgent = true;
        # localForwards = [{
        #   # syncthing
        #   bind.port = 8385;
        #   host.address = "127.0.0.1";
        #   host.port = 8384;
        # }];
      };
      "weisshorn" = {
        hostname = "5.34.251.181"; # TODO: Update DNS
        port = 2222;
        forwardAgent = true;
      };
      "liskamm" = {
        hostname = "ncoding.at";
        forwardAgent = true;
      };
      "remarkable" = {
        user = "root";
        extraOptions = {
          "PubkeyAcceptedKeyTypes" = "+ssh-rsa";
          "HostKeyAlgorithms" = "+ssh-rsa";
        };
      };
      "ambicam" = {
        user = "pi";
        forwardAgent = true;
        extraOptions = {
          "PubkeyAcceptedAlgorithms" = "+ssh-rsa";
        };
      };
      "ambilight" = {
        user = "root";
        forwardAgent = true;
      };
    };
  };
}
