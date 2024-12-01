{ pkgs, ... }:
{
  home.packages = with pkgs; [
    keepassxc # secret stuff
    bitwarden-desktop # new secret stuff
    bitwarden-menu
    bitwarden-cli # needed at least by bitwarden-menu
  ];

  # https://github.com/firecat53/bitwarden-menu/blob/main/docs/configure.md
  xdg.configFile."bwm/config.ini".source = ./config.ini;
}
