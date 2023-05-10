{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    arion.url = "github:hercules-ci/arion";
    emacsOverlay.url = "github:nix-community/emacs-overlay";

    # hydraw
    hydra.url = "github:input-output-hk/hydra/5642332bef2085974cf6936a47b2a406caba45d9";
    # The workbench flake inputs have some broken recursion in cardano-node
    # https://github.com/input-output-hk/cardano-node/pull/4865
    cardano-node = {
      url = "github:input-output-hk/cardano-node/1.35.5";
      inputs.node-measured.follows = "empty-flake";
      inputs.cardano-node-workbench.follows = "empty-flake";
    };
    empty-flake.url = "github:input-output-hk/empty-flake";
  };

  outputs = inputs: {
    nixosConfigurations = {
      eiger = import ./hosts/eiger inputs;
      matterhorn = import ./hosts/matterhorn inputs;
      liskamm = import ./hosts/liskamm inputs;
    };
  };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://hydra-node.cachix.org"
      "https://cardano-scaling.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "hydra-node.cachix.org-1:vK4mOEQDQKl9FTbq76NjOuNaRD4pZLxi1yri31HHmIw="
      "cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY="
    ];
    allow-import-from-derivation = true;
  };
}
