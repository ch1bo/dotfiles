{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    arion.url = "github:hercules-ci/arion";
  };

  outputs = { self, nixpkgs, arion, ... }: {
    nixosConfigurations.liskamm = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit nixpkgs; };
      modules = [
        arion.nixosModules.arion
        ./hosts/liskamm/configuration.nix
      ];
    };
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://hydra-node.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hydra-node.cachix.org-1:vK4mOEQDQKl9FTbq76NjOuNaRD4pZLxi1yri31HHmIw="
    ];
    allow-import-from-derivation = true;
  };
}
