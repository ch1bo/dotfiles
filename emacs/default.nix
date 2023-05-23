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
  home.sessionPath = [ "${emacsDir}/bin" ];
  # symlink config & doom emacs.d
  home.activation.doomEmacs = lib.hm.dag.entryAfter ["writeBoundary"] ''
      $DRY_RUN_CMD ln -sfT $VERBOSE_ARG ${emacsDir}/doom.emacs.d/ $HOME/.emacs.d
      $DRY_RUN_CMD ln -sfT $VERBOSE_ARG ${emacsDir}/doom.d/ $HOME/.doom.d
    '';

  home.packages = [
    pkgs.emacsNativeComp # The editor (native branch)

    # doom-emacs dependencies
    pkgs.fd
    pkgs.git
    pkgs.ripgrep
  ];
}
