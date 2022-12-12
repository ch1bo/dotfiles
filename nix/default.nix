{ config, pkgs, ... }:

let
  # TODO use nix flakes
  unstable = import
    (builtins.fetchTarball {
      # Descriptive name to make the store path easier to identify
      name = "nixpkgs-unstable-2022-04-25";
      url = "https://github.com/nixos/nixpkgs/archive/6a323903ad07de6680169bb0423c5cea9db41d82.tar.gz";
      sha256 = "0mz0kamp3k8py6hswpnrv1fm3jav48m76pzaw6r7ic2r837bnpw9";
    })
    { };
in

{

  # Also use config files for nix-env etc.
  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
  nixpkgs.overlays = import ./nixpkgs-overlays.nix ++ [
    (self: super: {
      inherit unstable;
      direnv = unstable.direnv;
      nix-direnv = unstable.nix-direnv;
    })
  ];
  xdg.configFile."nixpkgs/overlays.nix".source = ./nixpkgs-overlays.nix;

  # TODO nix.conf holds substituters and should be writable by e.g. 'cachix use'
  # Possible solution: script to quickly 'swap' ./nix.conf with
  # .config/nix/nix.conf for the occasional use.
  xdg.configFile."nix/nix.conf".source = ./nix.conf;

  # Provide a compatible locale-archive to nix applications using
  # LOCALE_ARCHIVE. This avoids problems where the host's glibc is newer and
  # locales are incompatible.
  home.sessionVariables = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };

  # Locate missing commands using nix-index
  programs.zsh.initExtra = ''
    source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
  '';

  home.packages = [
    pkgs.cachix # nix cache
    pkgs.nix-index # locate for nix
    pkgs.nixfmt # format nix
    pkgs.nixpkgs-fmt # format nix the nixpkgs way
  ];
}
