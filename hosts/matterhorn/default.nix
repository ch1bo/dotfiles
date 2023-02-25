inputs@{ nixpkgs, ... }:
# Build a nixos system using given nixpkgs
nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
  specialArgs = { inherit inputs system; };
  modules = [
    ./configuration.nix
  ];
}
