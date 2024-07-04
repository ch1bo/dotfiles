inputs@{ nixpkgs, nixpkgs-unstable, nixpkgs-2311, home-manager, ... }:
# Build a nixos system using stable nixpkgs
nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
  specialArgs = {
    inherit inputs system;
    pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
  };
  modules = [
    # System configuration
    ./configuration.nix
    # The home-manager module
    home-manager.nixosModules.home-manager
    # Configure home-manager to use global system config
    ({ config, system, ... }: {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      # Additional arguments used in home.nix
      home-manager.extraSpecialArgs = {
        unstable = import nixpkgs-unstable {
          config = config.nixpkgs.config;
          inherit system;
        };
        pkgs-2311 = import nixpkgs-2311 {
          config = config.nixpkgs.config;
          inherit system;
        };
      };
      # XXX: user name needs to match multiple places
      home-manager.users.ch1bo = import ./home.nix;
    })
  ];
}
