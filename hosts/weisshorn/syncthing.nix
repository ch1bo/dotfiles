{ config, lib, pkgs, ... }:
{
  services.syncthing = {
    enable = true;
    user = config.user.name;
    dataDir = config.user.home;
    openDefaultPorts = true;
  };

  # TODO: configure gui password via sops or age
  services.syncthing.guiAddress = "0.0.0.0:8384";
  networking.firewall.allowedTCPPorts = [ 8384 ];

  # Declarative directories
  services.syncthing.settings = {
    devices = {
      "liskamm" = { id = "MK5ZVT5-OAMBOEC-LBEQS2F-XZDNIZ6-QQWIKKY-HONRTH6-L77BB2T-BJTMQQ5"; };
      "eiger" = { id = "VSSBZ66-ROXY72N-2TSLXO3-ZJVG45G-RWPQT5C-5PERMGB-HDCNV5T-XWO2WAM"; };
      "matterhorn" = { id = "7TRMVFR-4XZWTSP-FFIZVO4-NTJRWFH-HMWDM3D-FRLU5WJ-R44IRF3-GSUNJQ6"; };
      "pixel6a" = { id = "L3QAZ5K-E3AHY32-E6BY5G5-WK5LDRT-46J6Y6M-VNOODFT-FGRHYZC-KG7AQAJ"; };
    };
    folders = {
      "dotfiles" = {
        id = "d6cet-yojnm";
        path = "/home/ch1bo/.dotfiles";
        devices = [ "liskamm" "eiger" "matterhorn" ];
      };
      "obsidian" = {
        id = "p2bc3-tuajo";
        path = "/home/ch1bo/obsidian";
        devices = [ "liskamm" "eiger" "matterhorn" ];
      };
    };
  };

  # TODO: also make node id deterministic
  # https://wiki.nixos.org/wiki/Syncthing#Declarative_node_IDs
}
