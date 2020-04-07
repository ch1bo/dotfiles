let
  pkgs = import <nixpkgs> { };

  essentials = (with pkgs; [
    feh # for the backgrounds
    fzf # fuzzy find tool
    git # bootstrap from nix
    glibcLocales # non broken LOCALE
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
    # TODO(SN): share binary not in release -> overlay? fetchGit ch1bo/nixpkgs?
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
in essentials ++ nicetohave
