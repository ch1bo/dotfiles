{ config, pkgs, ... }:
{
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
  programs.zsh.initContent = ''
    source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
  '';

  home.packages = [
    pkgs.cachix # nix cache
    pkgs.nix-index # locate for nix
    pkgs.nixfmt # format nix
    pkgs.nil # nix lsp
  ];
}
