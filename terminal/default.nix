{ config, lib, pkgs, ... }:

{
  programs.alacritty.enable = true;
  programs.alacritty.settings = {
    font = {
      size = 14;
      normal.family = "Hasklug Nerd Font";
    };

    scrolling.history = 100000;

    hints.enabled = [
      # Copy paths
      {
        binding = { key = "P"; mods = "Control|Shift"; };
        action = "copy";
        regex = ''(\\/|~|\\.\\/|\\.\\.\\/)[\\w./?&@#-]+'';
        post_processing = true;
      }
      # Open links
      {
        binding = { key = "O"; mods = "Control|Shift"; };
        command = "xdg-open";
        hyperlinks = true;
        mouse.enabled = true;
        mouse.mods = "Control";
        post_processing = true;
        persist = false;
        regex = ''(ipfs:|ipns:|magnet:|mailto:|gemini://|gopher://|https://|http://|news:|file:|git://|ssh:|ftp://)[^\u0000-\u001F\u007F-\u009F<>"\\s{-}\\^⟨⟩‘]+'';
      }
    ];

    # NOTE: Color scheme defined by zsh. See shell/colors.zsh
    colors.draw_bold_text_with_bright_colors = true;
    # NOTE: Also set here to not have wrong colors until zsh loads.
    colors.primary = {
      background = "#282c34";
      foreground = "#abb2bf";
    };
    colors.normal = {
      black = "#1e2127";
      red = "#e06c75";
      green = "#98c379";
      yellow = "#d19a66";
      blue = "#61afef";
      magenta = "#c678dd";
      cyan = "#56b6c2";
      white = "#abb2bf";
    };

    colors.bright = {
      black = "#5c6370";
      red = "#e06c75";
      green = "#98c379";
      yellow = "#d19a66";
      blue = "#61afef";
      magenta = "#c678dd";
      cyan = "#56b6c2";
      white = "#ffffff";
    };
  };

  home.packages = [
    pkgs.xsel # clipboard
  ];
}
