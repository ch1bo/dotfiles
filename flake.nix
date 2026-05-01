{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # system inputs
    nix-alien.url = "github:thiagokokada/nix-alien";
    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake/beta";
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };

    # ncoding
    cv.url = "github:ch1bo/cv";
    laendlefinder.url = "github:ch1bo/laendlefinder";

    # hydraw
    hydra.url = "github:input-output-hk/hydra/0.21.0";
    cardano-node.url = "github:intersectmbo/cardano-node/10.4.1";
    mithril.url = "github:input-output-hk/mithril/2517.1";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      hosts = builtins.attrNames self.nixosConfigurations;

      deployScript = pkgs.writeShellScriptBin "deploy" ''
        set -euo pipefail
        HOST=''${1:-$(hostname)}
        FLAKE=".?submodules=1#$HOST"
        REMOTE_ARGS=()
        if [ "$HOST" != "$(hostname)" ]; then
          REMOTE_ARGS=(--target-host "$HOST" --build-host "$HOST")
        fi

        echo "Building $HOST..."
        nixos-rebuild build --flake "$FLAKE" \
          "''${REMOTE_ARGS[@]}" |& nom

        echo ""
        echo "Diff against running system:"
        if [ ''${#REMOTE_ARGS[@]} -gt 0 ]; then
          nvd diff "$(ssh "$HOST" readlink /run/current-system)" ./result
        else
          nvd diff /run/current-system ./result
        fi

        echo ""
        read -p "Switch $HOST? [y/N] " -n 1 -r; echo
        [[ $REPLY =~ ^[Yy]$ ]] && nixos-rebuild switch --flake "$FLAKE" \
          "''${REMOTE_ARGS[@]}" |& nom
      '';

      statusScript = pkgs.writeShellScriptBin "status" ''
        set -euo pipefail
        HOSTS="${builtins.concatStringsSep " " hosts}"
        CURRENT=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
        echo "Flake HEAD: $CURRENT"
        echo ""
        printf "%-15s %-12s %s\n" "HOST" "DEPLOYED" "BEHIND"
        printf "%-15s %-12s %s\n" "----" "--------" "------"
        for host in $HOSTS; do
          rev=$(ssh -o ConnectTimeout=3 "$host" nixos-version --configuration-revision 2>/dev/null || echo "unreachable")
          if [ "$rev" = "unreachable" ]; then
            printf "%-15s %-12s %s\n" "$host" "unreachable" "-"
          else
            short=''${rev:0:7}
            behind=$(git rev-list --count "''${rev}..HEAD" 2>/dev/null || echo "?")
            printf "%-15s %-12s %s\n" "$host" "$short" "$behind"
          fi
        done
      '';
    in
    {
      nixosConfigurations = {
        eiger = import ./hosts/eiger inputs;
        matterhorn = import ./hosts/matterhorn inputs;
        liskamm = import ./hosts/liskamm inputs;
        weisshorn = import ./hosts/weisshorn inputs;
      };

      devShells.${system}.default = pkgs.mkShell {
        packages = [
          pkgs.nvd
          pkgs.nix-output-monitor
          deployScript
          statusScript
        ];
        shellHook = ''
          echo "dotfiles — available commands:"
          echo "  status        Show deployed revisions on all hosts"
          echo "  deploy <host> Build, diff, and switch a remote host"
        '';
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
