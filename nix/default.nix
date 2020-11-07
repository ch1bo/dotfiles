{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.nixpkgs-fmt
  ];

  # Use default paths instead of config.nixpkgs for nix-env etc.
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/overlays.nix".source = ./nixpkgs-overlays.nix;

  # Provide a compatible locale-archive to nix applications using
  # LOCALE_ARCHIVE. This avoids problems where the host's glibc is newer and
  # locales are incompatible.
  home.sessionVariables = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };
}
