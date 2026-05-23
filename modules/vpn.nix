# VPN using wireguard via network manager for on-demand connections. Anything
# persistent should be defined declaratively, see also:
# https://nixos.wiki/wiki/WireGuard
{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.wireguard-tools ];

  # rpfilter drops packets arriving on wg0 because their source (10.5.5.x) does
  # not match the routing table for the default interface.
  networking.firewall.checkReversePath = false;

  # Generate keys:
  # > umask 077
  # > mkdir wg-keys
  # > wg genkey > wg-keys/private
  # > wg pubkey < wg-keys/private > wg-keys/public

  # Create a client.conf file (or get it from the vpn server):
  # > [Interface]
  # > # IP on the wireguard network
  # > Address = 10.5.5.3/24
  # > PrivateKey = 0000000000000000000000000000000000000000000=
  # >
  # > [Peer]
  # > PublicKey = 1111111111111111111111111111111111111111111=
  # > # restrict this to the wireguard subnet if you don't want to route everything to the tunnel
  # > AllowedIPs = 0.0.0.0/0
  # > # ip and port of the server
  # > Endpoint = vpn.ncoding.li:51820

  # Import configuration to network manager:
  # > nmcli connection import type wireguard file client.conf
}
