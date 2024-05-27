{ pkgs, ... }:

{
  # TODO: Add config via home-manager
  # xdg.configFile."obsidian/obsidian.json"

  environment.systemPackages = with pkgs; [
    obsidian
  ];

  nixpkgs.config.permittedInsecurePackages = [
    "electron-25.9.0"
  ];
}
