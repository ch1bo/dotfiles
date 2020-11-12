# Main module for my machine used at work at FE
# OS: Ubuntu 16.04
# DM: LightDM
{
  imports = [
    ./common.nix
    ./mail/account-franka.nix
  ];

  home.username = "nage_se";
  wifi = "wlp1s0";

  # Start the settings daemon on ubuntu
  xsession.initExtra = ''
    if [ $(which unity-settings-daemon 2> /dev/null) ]; then
      unity-settings-daemon &
    fi
  '';
}
