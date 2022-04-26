let
  emacsOverlay = import (builtins.fetchGit {
    url = "https://github.com/nix-community/emacs-overlay.git";
    ref = "master";
    rev = "5f258dfdab8e58abe2e44b78a3ccf262041f7b74";
  });
in
[ emacsOverlay ]
