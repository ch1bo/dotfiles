let
  emacsOverlay = import (builtins.fetchGit {
    url = "https://github.com/nix-community/emacs-overlay.git";
    ref = "master";
    rev = "5ceb4357c51dffd7f657d8baa3e0249bb1b61527";
  });
in
[ emacsOverlay ]
