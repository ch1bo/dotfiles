{ config, pkgs, ... }:

{
  programs.gpg.enable = true;
  # Hardened configuration from github.com/drduh/config
  home.file."${config.home.homeDirectory}/.gnupg/gpg.conf".source = ./gpg.conf;

  # TODO: GPG agent replicates ssh keys from keepassxc into
  # .gnupg/private-keys-v1.d
  services.gpg-agent = {
    enable = true;
    # NOTE: This is incompatible with agents = [ "ssh" ] of keychain
    enableSshSupport = true;
    # Used for agent forwarding
    enableExtraSocket = true;
    # HACK: faked time to keep using the expired keys until refreshed
    extraConfig = ''
      faked-system-time 20220201T151400
    '';
  };

  # Manage SSH and GPG agents
  programs.keychain = {
    enable = true;
    # NOTE: Disable SSH agent as we use the GPG agent for that
    agents = [ "gpg" ];
    # Keys are added manually or via keepassxc
    keys = [ ];
  };

  # Mainly for mailvelope
  home.packages = [ pkgs.gpgme ];
  home.file.".mozilla/native-messaging-hosts/gpgmejson.json".text = ''
    {
      "name": "gpgmejson",
      "description": "Integration with GnuPG",
      "path": "${pkgs.gpgme.dev}/bin/gpgme-json",
      "type": "stdio",
      "allowed_extensions": [
        "jid1-AQqSMBYb0a8ADg@jetpack"
      ]
    }
  '';
}
