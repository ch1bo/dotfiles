{ pkgs, ... }:

{
  # TODO: Add config via home-manager
  # xdg.configFile."obsidian/obsidian.json"

  environment.systemPackages = with pkgs; [
    obsidian
  ];
}
