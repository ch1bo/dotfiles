{ pkgs-unstable, ... }:
{
  services.udev.packages = [ pkgs-unstable.bazecor ];
  environment.systemPackages = [ pkgs-unstable.bazecor ];
}
