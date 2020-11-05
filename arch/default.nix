{ config, pkgs, ... }:

{
  home.packages = [
    (import ./slockArch.nix { inherit pkgs; }).slockArch
  ];
}
