inputs@{
  nixpkgs,
  nixpkgs-unstable,
  home-manager,
  ...
}:
# Build a nixos system using given nixpkgs
nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
  specialArgs = {
    inherit inputs system;
  };
  modules = [
    # Overlay pkgs.unstable with same config as pkgs. This notably re-uses the
    # allowUnfree config and allows the same unfree pkgs in unstable.
    (
      { config, ... }:
      {
        nixpkgs.overlays = [
          (final: prev: {
            unstable = import nixpkgs-unstable {
              inherit system;
              inherit (config.nixpkgs) config;
            };
          })
        ];
      }
    )
    # System configuration
    ./configuration.nix
    # The home-manager module
    home-manager.nixosModules.home-manager
    # Configure home-manager to use global system config
    (
      { config, ... }:
      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          backupFileExtension = "bkp";
          # Additional arguments used in home.nix
          extraSpecialArgs = {
            inherit inputs;
          };
          users.${config.user.name} = {
            home.username = config.user.name;
          }
          // import ./home.nix;
        };
      }
    )
  ];
}
