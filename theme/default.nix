# Arc Dark theme with Numix cursors throughout x11, gtk2 and gtk3 applications
# along with doom/atom one dark color scheme
#
# TODO: turn configuration around? i.e. set colors, font etc. as read-only
# options here and use it in individual modules?
{ pkgs, lib, ... }:
{
  gtk = {
    enable = true;
    theme = {
      package = pkgs.arc-theme;
      name = "Arc-Dark";
    };
    iconTheme = {
      package = pkgs.arc-icon-theme;
      name = "Arc";
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
      gtk-toolbar-style = "GTK_TOOLBAR_BOTH";
      gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";
      gtk-button-images = 1;
      gtk-menu-images = 1;
      gtk-enable-event-sounds = 1;
      gtk-enable-input-feedback-sounds = 1;
      gtk-xft-antialias = 1;
      gtk-xft-hinting = 1;
      gtk-xft-hintstyle = "hintfull";
      gtk-xft-rgba = "rgb";
    };
  };
  home.sessionVariables.GTK_THEME = "Arc-Dark";

  programs.rofi = {
    theme = "Arc-Dark";
    font = "Hasklug Nerd Font 14";
  };

  home.pointerCursor = {
    package = pkgs.numix-cursor-theme;
    name = "Numix-Cursor";
    x11.enable = true;
  };

  programs.vim.extraConfig = ''
    let base16colorspace=256
    colorscheme base16-onedark
  '';
  home.file.".vim/colors".source = ./vim-colors;
}
