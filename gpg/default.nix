{ config, pkgs, ... }:

{
  programs.gpg.enable = true;

  # Hardened configuration from github.com/drduh/config
  home.file."${config.home.homeDirectory}/.gnupg/gpg.conf".source = ./gpg.conf;

  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry-gnome3;
    enableSshSupport = true;
    sshKeys = [ "B73C82125079C8FC7966 6FFA59FAA903C906659A" ];
    # Used for agent forwarding
    enableExtraSocket = true;
  };
}
