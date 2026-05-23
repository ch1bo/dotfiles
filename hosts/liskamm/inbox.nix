# Samba "inbox" share for the network scanner/MFP. Drop point only — files are
# expected to be moved/processed elsewhere (e.g. paperless, manual sorting).
#
# Guest-writable, LAN-only. We rely on the network boundary (firewall scoped to
# the LAN interface, hosts allow set to RFC1918) for access control rather than
# SMB credentials, since the only client is the MFP we control.
let
  inboxPath = "/data/inbox";
  lanInterface = "enp0s31f6";
  # The Nextcloud container's Apache runs as uid 33 (www-data) inside. Pin a
  # matching user on the host so files dropped by samba are owned by the same
  # uid Nextcloud sees, no chmod gymnastics required.
  shareUser = "www-data";
  shareUid = 33;
in
{
  users.users.${shareUser} = {
    isSystemUser = true;
    group = shareUser;
    uid = shareUid;
  };
  users.groups.${shareUser} = {
    gid = shareUid;
  };

  systemd.tmpfiles.rules = [
    "d ${inboxPath} 0775 ${shareUser} ${shareUser} - -"
  ];

  services.samba = {
    enable = true;
    openFirewall = false; # scoped to the LAN interface below
    settings = {
      global = {
        "workgroup" = "WORKGROUP";
        "server string" = "liskamm";
        "server role" = "standalone server";
        "map to guest" = "bad user";
        # MFPs are often stuck on SMB1; raise the floor if yours can do SMB2+.
        "server min protocol" = "SMB2";
        "interfaces" = "lo ${lanInterface}";
        "bind interfaces only" = "yes";
        "hosts allow" = "192.168. 127.";
        "hosts deny" = "0.0.0.0/0";
      };

      inbox = {
        "path" = inboxPath;
        "browseable" = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "guest only" = "yes";
        "force user" = shareUser;
        "force group" = shareUser;
        "create mask" = "0664";
        "directory mask" = "0775";
      };
    };
  };

  # WS-Discovery so Windows/macOS see the share.
  services.samba-wsdd.enable = true;

  networking.firewall.interfaces.${lanInterface} = {
    allowedTCPPorts = [
      139
      445
    ];
    allowedUDPPorts = [
      137
      138
      3702
    ];
  };
}
