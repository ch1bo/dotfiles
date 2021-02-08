{ config, pkgs, ... }:

{
  programs.gpg.enable = true;
  # Hardened configuration from github.com/drduh/config
  home.file.".gnupg/gpg.conf".source = ./gpg.conf;

  # Manage SSH and GPG agents
  programs.keychain = {
    enable = true;
    keys = [ ]; # Added by hand or via keepassxc
  };
}
