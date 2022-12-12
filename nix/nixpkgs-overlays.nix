# TODO: use a flake input and make it easier to bump this (plus use their binary cache)
let
  emacsOverlay = import (builtins.fetchGit {
    url = "https://github.com/nix-community/emacs-overlay.git";
    ref = "master";
    rev = "61e8c3167cd2a748a7a805caca3ae5756b2b6eb5";
  });
in
[ emacsOverlay ]
