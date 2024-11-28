{ pkgs, ... }:
{
  services.udev.packages = [ pkgs.bazecor ];
  environment.systemPackages = [ pkgs.bazecor ];
}
