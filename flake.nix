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
    jail-nix.url = "sourcehut:~alexdavid/jail.nix";
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
        LOCAL=$(hostname)

        echo "Building $HOST..."
        if [ "$HOST" != "$LOCAL" ]; then
          ssh -o ConnectTimeout=10 "$HOST" true
          RESULT=$(nixos-rebuild build --flake "$FLAKE" \
            --target-host "$HOST" --build-host "$HOST" \
            2> >(nom >&2) \
            | grep -oP '/nix/store/\S+-nixos-system-\S+' | tail -1)
        else
          nixos-rebuild build --flake "$FLAKE" |& nom
          RESULT=$(readlink -f ./result)
        fi

        echo ""
        echo "Diff against running system:"
        if [ "$HOST" != "$LOCAL" ]; then
          ssh -t "$HOST" nvd diff /run/current-system "$RESULT"
        else
          nvd diff /run/current-system "$RESULT"
        fi

        echo ""
        read -p "Switch $HOST? [y/N] " -n 1 -r; echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
          if [ "$HOST" != "$LOCAL" ]; then
            ssh -t "$HOST" sudo nix-env -p /nix/var/nix/profiles/system --set "$RESULT" \
              \&\& sudo "$RESULT/bin/switch-to-configuration" switch
          else
            sudo nix-env -p /nix/var/nix/profiles/system --set "$RESULT"
            sudo "$RESULT/bin/switch-to-configuration" switch
          fi
        fi
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
          if [ "$host" = "$(hostname)" ]; then
            rev=$(nixos-version --configuration-revision 2>/dev/null || true)
          else
            rev=$(ssh -o ConnectTimeout=3 "$host" nixos-version --configuration-revision 2>/dev/null || true)
          fi
          if [ -z "$rev" ]; then
            if [ "$host" = "$(hostname)" ]; then
              printf "%-15s %-12s %s\n" "$host" "unknown" "-"
            else
              printf "%-15s %-12s %s\n" "$host" "unreachable" "-"
            fi
          else
            rev=''${rev%-dirty}
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
