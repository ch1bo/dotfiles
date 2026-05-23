# Persistent VPN connection to ncoding.li
# See hosts/liskamm/README.org for instructions
{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.wireguard-tools ];

  # rpfilter drops packets arriving on wg0 because their source (10.5.5.x) does
  # not match the routing table for the default interface.
  networking.firewall.checkReversePath = false;

  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.5.5.2/24" ];
    privateKeyFile = "/root/keys/wg/private";
    peers = [
      {
        publicKey = "XdhEMVqHeR5NWNcpZlWLKaf2bI2G5399o1gK/uOzYGw=";
        allowedIPs = [ "10.5.5.0/24" ];
        endpoint = "vpn.ncoding.li:51820";
        # Keep the NAT mapping alive
        persistentKeepalive = 25;
      }
    ];
  };
}
