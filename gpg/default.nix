{ config, pkgs, ... }:

{
  programs.gpg.enable = true;
  # Hardened configuration from github.com/drduh/config
  home.file.".gnupg/gpg.conf".source = ./gpg.conf;

  services.gpg-agent = {
    enable = true;
    # TODO: This is incompatible with agents = [ "ssh" ] of keychain
    enableSshSupport = true;
  };

  # Manage SSH and GPG agents
  programs.keychain = {
    enable = true;
    # NOTE: Disable SSH agent as we use the GPG agent for that
    agents = [ "gpg" ];
    # Keys are added manually or via keepassxc
    keys = [ ];
  };

  # TODO: GPG agent replicates ssh keys from keepassxc into
  # .gnupg/private-keys-v1.d
}
