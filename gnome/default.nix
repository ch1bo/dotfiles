{ config, pkgs, ... }:
{
  # Arc Dark theme with Numix cursors throughout x11, gtk2 and gtk3 applications
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

  xsession.pointerCursor = {
    package = pkgs.numix-cursor-theme;
    name = "Numix-Cursor";
  };
  # TODO upstream into home-manager xcursor module
  home.file.".icons/default".source = "${pkgs.numix-cursor-theme}/share/icons/Numix-Cursor";

  home.packages = [
    pkgs.gnome3.eog
    pkgs.gnome3.evince
    pkgs.gnome3.nautilus
  ];
}
