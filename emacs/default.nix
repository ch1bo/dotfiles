# Module which installs and configures my various emacs distributions in use. As
#
# I am in the process of switching between spacemacs to doom-emacs, I use the
# chemacs profile switcher + utilty scripts to launch these two profiles.
#
# Furthermore, we deliberately set up emacs up to look for config and emacs
# distribution upstreams within the dotfiles working copy to allow for faster
# tweaking (for now at least).
#
# TODO emacs-overlay with emacsGcc
# TODO dependencies like git, fd, rg, etc.

{ config, pkgs, ... }:
let
  emacsDir = "${config.dotfiles}/emacs";
  # The 'doom' utility
  doom = pkgs.writeScriptBin "doom" ''
    EMACSDIR=${emacsDir}/doom.emacs.d \
    DOOMDIR=${emacsDir}/doom.d \
    ${emacsDir}/doom.emacs.d/bin/doom "$@"
  '';
  # Chemacs shortcuts
  doom-emacs = pkgs.writeScriptBin "doom-emacs" ''
    exec emacs --with-profile doom
  '';
  spacemacs = pkgs.writeScriptBin "spacemacs" ''
    emacsclient - -alternate-editor="emacs --with-profile spacemacs" -s spacemacs -c "$@"
  '';
in
{
  # Chemacs profile switcher with user-configuration directly in working copy
  home.file.".emacs".source = ./chemacs;
  home.file.".emacs-profiles.el".text = ''
    (("default" . ((user-emacs-directory . "${emacsDir}/doom.emacs.d")
                   (env . (("DOOMDIR" . "${emacsDir}/doom.d")))))
     ("doom" . ((user-emacs-directory . "${emacsDir}/doom.emacs.d")
                (env . (("DOOMDIR" . "${emacsDir}/doom.d")))))
     ("spacemacs" . ((user-emacs-directory . "${emacsDir}/spacemacs.emacs.d")
                     (server-name . "spacemacs")
                     (env . (("SPACEMACSDIR" . "${emacsDir}/spacemacs.d"))))))
  '';

  home.packages = [
    pkgs.emacs # The editor

    # Utilities
    doom
    doom-emacs
    spacemacs

    # doom-emacs dependencies
    pkgs.fd
    pkgs.git
    pkgs.ripgrep
  ];
}
