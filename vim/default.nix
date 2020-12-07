{ config, pkgs, ... }:
{
  programs.vim = {
    enable = true;
    plugins = [
      pkgs.vimPlugins.syntastic
      pkgs.vimPlugins.vim-nix
    ];
    settings = {
      modeline = true;
      shiftwidth = 2;
      tabstop = 2;
      expandtab = true;
      background = "dark";
    };
    # NOTE Color scheme is set in theme module
    extraConfig = ''
      set nocompatible
      syntax on
      filetype plugin indent on

      " Map ctrl+arrow keys
      nnoremap Oa k
      nnoremap Ob j
      nnoremap Oc w
      nnoremap Od b
      inoremap Oa <ESC>ka
      inoremap Ob <ESC>ja
      inoremap Oc <ESC>lwi
      inoremap Od <ESC>lbi

      " Syntastic
      set statusline+=%#warningmsg#
      set statusline+=%{SyntasticStatuslineFlag()}
      set statusline+=%*

      let g:syntastic_always_populate_loc_list = 1
      let g:syntastic_auto_loc_list = 0
      let g:syntastic_check_on_open = 1
      let g:syntastic_check_on_wq = 0
    '';
  };
}
