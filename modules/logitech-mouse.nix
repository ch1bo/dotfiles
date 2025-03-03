{ pkgs, ... }:
{
  services.ratbagd.enable = true;
  services.ratbagd.package = pkgs.callPackage ./libratbag { };
  environment.systemPackages = [ pkgs.piper ];
}
