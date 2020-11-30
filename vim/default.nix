{ config, pkgs, ... }:
{
  programs.vim = {
    enable = true;
    plugins = [ pkgs.vimPlugins.syntastic ];
  };
  # TODO configure using the vim module instead?
  home.file.".vimrc".source = ./vimrc;
  home.file.".vim/colors".source = ./colors;
}
