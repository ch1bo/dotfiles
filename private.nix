# Main module for my private Dell XPS 13 notebook
# OS: Arch
# DM: LightDM
{
  imports = [
    ./common.nix
    ./mail/account-ncoding.nix
  ];

  home.username = "ch1bo";
  wifi = "wlp58s0";
}
