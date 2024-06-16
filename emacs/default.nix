# Module which installs and configures doom emacs.
#
# This setup deliberately keeps the config and emacs distribution upstreams
# within the dotfiles working copy to allow for faster tweaking.
{ config, pkgs, pkgs-2311, lib, unstable, ... }:
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
  programs.emacs.package = pkgs.emacsNativeComp;
  # Pre-install mu4e 1.10 as doom-emacs has still problems with latest 1.12
  programs.emacs.extraPackages = epkgs: [
    pkgs-2311.emacsPackages.mu4e
  ];

  # Dependencies
  home.packages = [
    pkgs.fd
    pkgs.git
    pkgs.ripgrep
    # Needed for copilot
    pkgs.nodejs
    # Using unstable for most experimental diagram support
    unstable.nodePackages.mermaid-cli
  ];
}
