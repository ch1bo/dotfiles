{ ... }:
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "eiger" = {
        hostname = "192.168.2.30";
        forwardAgent = true;
      };
      "weisshorn" = {
        hostname = "192.168.2.238";
        forwardAgent = true;
      };
      "weisshorn-ext" = {
        hostname = "vpn.ncoding.li";
        port = 2222;
        forwardAgent = true;
      };
      "liskamm-ext" = {
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
