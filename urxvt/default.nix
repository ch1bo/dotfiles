{ config, pkgs, ... }:

{
  # TODO use programs.urxvt module?
  # NOTE Configuration in x/xresources.

  home.packages = [
    pkgs.rxvt-unicode # the terminal
    pkgs.xsel # clipboard
  ];

  # Extensions
  home.file.".urxvt/ext".source = ./ext;

  # zsh has problems with the urxvt default 'rxvt-unicode-256color'
  programs.zsh.sessionVariables = {
    TERM = "xterm-256color";
  };
}
