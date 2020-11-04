{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userName = "Sebastian Nagel";
    userEmail = "sebastian.nagel@ncoding.at";

    aliases = {
      co = "checkout";
      graph = "log --graph --oneline --decorate";
      changelog = "log --pretty=format:'%s' --no-merges";
      count = "shortlog -sn";
      wtf = "!$DOTFILES/bin/git-wtf";
      up = "!$DOTFILES/bin/git-up";
      dlm = "!$DOTFILES/bin/git-delete-local-merged";
      sup = "!$DOTFILES/bin/git-sup";
    };

    ignores = [
      "*~"
      "*.swp"
      ".clang_complete"
      "TAGS"
      "dist*/"
    ];

    extraConfig = {
      color.ui = true;
      diff.submodule = "log";
      fetch.recurseSubmodules = "on-demand";
      pull.rebase = "preserve";
      push.default = "simple";
      rerere = {
        enabled = true;
        autoupdate = true;
      };
      status.submoduleSummary = true;
    };
  };

  programs.zsh.shellAliases = {
    gf = "git fetch --prune";
    gl = "git graph";
    gla = "git graph --all";
    gd = "git diff";
    gc = "git commit";
    ga = "git add -p";
    gco = "git checkout";
    gb = "git branch";
    gs = "git status -sb";
  };
}
