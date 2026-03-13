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

  # Automatically creates a loader in /lib/* to avoid patching stuff
  # To disable it temporarily use
  # unset NIX_LD
  programs.nix-ld.enable = true;
}
