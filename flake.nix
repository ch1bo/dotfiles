{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # ncoding
    cv.url = "github:ch1bo/cv";

    # hydraw
    hydra.url = "github:input-output-hk/hydra/4421f52313b109281a47e38c3fecc82bbb9baa7a";
    cardano-node.url = "github:intersectmbo/cardano-node/10.1.3";
    mithril.url = "github:input-output-hk/mithril/2450.0";
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
      "https://ch1bo.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://cardano-scaling.cachix.org"
    ];
    extra-trusted-public-keys = [
      "ch1bo.cachix.org-1:uFj+YZChhtGGAC8/Reo3MNGK2rFfAkfeir51zuffH2Q="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cardano-scaling.cachix.org-1:QNK4nFrowZ/aIJMCBsE35m+O70fV6eewsBNdQnCSMKA="
    ];
    allow-import-from-derivation = true;
  };
}
