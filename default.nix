{ pkgs ? import <nixpkgs> {} }:

let
  #NOTE(SN): https://github.com/NixOS/nixpkgs/pull/84627
  zsh-syntax-highlighting = pkgs.callPackage (import (builtins.fetchurl {
    name = "nixpkgs-ch1bo-zsh-sytnax-highlighting-share";
    url = "https://raw.githubusercontent.com/ch1bo/nixpkgs/b8a212f625b5eebe3c28a62c6736674cd516dd40/pkgs/shells/zsh/zsh-syntax-highlighting/default.nix";
  })) {};

  slock = import ./nix/slockArch.nix;

  essentials = (with pkgs; [
    cachix # nix cache
    direnv # do dir specific things
    feh # for the backgrounds
    fzf # fuzzy find tool
    git # bootstrap from nix
    glibcLocales # non broken LOCALE
    lorri # keep nix-shell gc-roots
    nix # nix package manager
    rofi # launch stuff
    rxvt_unicode-with-plugins # urxvt terminal
    slock # suckless lock screen
    xbindkeys # launch commands from keys
    xmobar # status bar for xmonad
    xmonad-with-packages # the window manager
    xsel # clipboard
    xorg.setxkbmap # switch kb maps
    xorg.xrdb # for Xresources to theme urxvt
    xorg.xset # configure xorg
    zsh # the shell
    zsh-syntax-highlighting # like fish
  ]);

  nicetohave = (with pkgs; [
    bat # cat clone with wings
    chromium # browser
    emacs # the other editor
    fd # usable "find"
    mu # find mail
    nixfmt # format nix
    offlineimap # get mail
    pandoc # convert everything
    ripgrep # fast grep
    scrot # screenshots
    vim # the editor
  ]);
in  essentials ++ nicetohave
