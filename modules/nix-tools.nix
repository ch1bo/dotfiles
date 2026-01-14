{ inputs, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    nix-output-monitor
    nvd
    nix-tree
    nix-du
    inputs.nix-alien.packages.${system}.nix-alien
    patchelf
  ];
}
