# Module which installs and configures doom emacs.
#
# This setup deliberately keeps the config and emacs distribution upstreams
# within the dotfiles working copy to allow for faster tweaking.
{ config, pkgs, lib, ... }:
let
  emacsDir = "${config.dotfiles}/emacs";
in
{
  # doom binary
  home.sessionPath = [ "${emacsDir}/doom.emacs.d/bin" ];
  # symlink config & doom emacs.d
  home.activation.doomEmacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ln -sfT $VERBOSE_ARG ${emacsDir}/doom.emacs.d/ $HOME/.emacs.d
    $DRY_RUN_CMD ln -sfT $VERBOSE_ARG ${emacsDir}/doom.d/ $HOME/.doom.d
  '';

  programs.emacs.enable = true;
  # NOTE: home-manager emacs extraPackages is using pkgs.emacs by default and
  # seemingly the native comp version is older than normal emacs.
  programs.emacs.package = pkgs.emacsNativeComp.pkgs.withPackages (epkgs: with epkgs; [
    epkgs.mu4e
  ]);

  # Dependencies
  home.packages = [
    pkgs.fd
    pkgs.git
    pkgs.ripgrep
    # Needed for copilot
    pkgs.nodejs
    # Using unstable for most experimental diagram support
    pkgs.mermaid-cli
  ];
}
