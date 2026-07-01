# Temporary reverse proxy for services still running on liskamm (10.5.5.2).
# Weisshorn terminates TLS and proxies over the wireguard tunnel.
{ ... }:
let
  liskamm = "10.5.5.2";
  proxy = upstream: extra: {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://${upstream}";
      recommendedProxySettings = true;
      extraConfig = ''
        client_max_body_size 4G;
        # Increase timeouts for nextcloud
        proxy_connect_timeout 600s;
        proxy_send_timeout 600s;
        proxy_read_timeout 600s;
        fastcgi_send_timeout 600s;
        fastcgi_read_timeout 600s;
      '';
    }
    // extra;
  };
in
{
  services.nginx.virtualHosts."www.ncoding.at" = (proxy liskamm { }) // {
    serverAliases = [
      "ncoding.at"
      "ncoding.li"
      "www.ncoding.li"
    ];
  };
  services.nginx.virtualHosts."nextcloud.ncoding.at" = proxy liskamm { };
  services.nginx.virtualHosts."photos.ncoding.at" = proxy liskamm { proxyWebsockets = true; };
  services.nginx.virtualHosts."home.ncoding.at" = proxy liskamm { proxyWebsockets = true; };
  services.nginx.virtualHosts."passwords.ncoding.at" = proxy liskamm { };
  services.nginx.virtualHosts."laendlefinder.ncoding.at" = proxy liskamm { };
}
