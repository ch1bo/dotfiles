{ config, pkgs, ... }:

{
  programs.gpg.enable = true;
  # Hardened configuration from github.com/drduh/config
  home.file.".gnupg/gpg.conf".source = ./gpg.conf;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };

  # Manage SSH and GPG agents
  programs.keychain = {
    enable = true;
    # NOTE: Disable SSH agent as we use the GPG agent for that
    # TODO: "ssh" here is incompatible with gpg-agent.enableSshSupport
    agents = [ "gpg" ];
    # Keys are added manually or via keepassxc
    keys = [ ];
  };
}
