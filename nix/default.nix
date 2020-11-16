{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.nix
    pkgs.nix-index
    pkgs.nixpkgs-fmt
  ];

  # Use config files instead of config.nixpkgs module for nix-env etc.
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
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
}
