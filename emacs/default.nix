{ config, pkgs, ... }:
let
  # TODO how to avoid this hardcoded path?
  dotfilesPath = "$HOME/.dotfiles";
in
{
  home.packages = [ pkgs.emacs ];
  # Chemacs profile switcher
  home.file.".emacs".source = ./chemacs;
  home.file.".emacs-profiles.el".text = ''
    (("default" . ((user-emacs-directory . "${dotfilesPath}/emacs/doom.emacs.d")))
     ("doom" . ((user-emacs-directory . "${dotfilesPath}/emacs/doom.emacs.d")))
     ("spacemacs" . ((user-emacs-directory . "${dotfilesPath}/emacs/spacemacs.emacs.d"))))
  '';
  # TODO replace .dotfiles/bin path for doom, doom-emacs and spacemacs?
  home.file.".spacemacs".source = ./spacemacs;
  home.file.".doom.d".source = ./doom.d;
}
