{ config, pkgs, ... }:

{
  programs.gpg.enable = true;
  # Hardened configuration from github.com/drduh/config
  home.file."${config.home.homeDirectory}/.gnupg/gpg.conf".source = ./gpg.conf;

  # TODO: GPG agent replicates ssh keys from keepassxc into
  # .gnupg/private-keys-v1.d
  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry-gnome3;
    # NOTE: This is incompatible with agents = [ "ssh" ] of keychain
    enableSshSupport = true;
    # Used for agent forwarding
    enableExtraSocket = true;
  };
  # Always use gpg-agent as ssh agent
  home.sessionVariables.SSH_AUTH_SOCK = "/run/user/${config.home.uid or "1000"}/gnupg/S.gpg-agent.ssh";

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
