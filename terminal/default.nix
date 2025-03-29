{ config, lib, pkgs, ... }:

{
  programs.alacritty.enable = true;
  programs.alacritty.settings = {
    font = {
      size = 10;
      normal.family = "Hasklug Nerd Font";
    };

    scrolling.history = 100000;

    hints.enabled = [
      # Copy paths easily
      {
        regex = ''(\\/|~|\\.\\/|\\.\\.\\/)[\\w./?&@#-]+'';
        post_processing = true;
        action = "copy";
        binding.key = "P";
        binding.mods = "Control|Shift";
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
