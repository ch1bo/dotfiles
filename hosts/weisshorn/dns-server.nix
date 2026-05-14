# Self-hosted DNS server for dyndns
{
  services.powerdns.enable = true;
  services.powerdns.extraConfig = ''
    launch=gsqlite3
    gsqlite3-database=/data/powerdns/pdns.db
  '';
}
