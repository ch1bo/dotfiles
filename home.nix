{ config, pkgs, ... }:
let
  shellAliases = {
    l = "ls -CF";
    ll = "ls -alhF";
    la = "ls -A";
    ls = "ls --color=auto";
    grep = "grep --color=auto";
    fgrep = "fgrep --color=auto";
    egrep = "egrep --color=auto";
    vi = "vim";
    cert = "openssl x509 -noout";
  };
in
{
  programs.home-manager.enable = true;
  home.username = "ch1bo";
  home.homeDirectory = "/home/ch1bo";
  home.stateVersion = "20.09";

  home.packages = [ ];

  # nixpkgs for nix-env etc.
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs/config.nix;
  xdg.configFile."nixpkgs/overlays.nix".source = ./nixpkgs/overlays.nix;

  # shells
  programs.dircolors.enable = true;
  programs.bash = {
    enable = true;
    inherit shellAliases;
    profileExtra = builtins.readFile ./shell/profile;
    initExtra = builtins.readFile ./shell/bashrc;
  };
  programs.zsh = {
    enable = true;
    inherit shellAliases;
    history = {
      size = 10000000;
      save = 10000000;
    };
    profileExtra = builtins.readFile ./shell/profile;
    initExtraBeforeCompInit = ''
      # Generic .zshrc which sources all *.zsh files in the dotfiles repository (completion.zsh last)
      typeset -U config_files
      config_files=($DOTFILES/*/*.zsh)

      # add each topic folder to fpath so that they can add functions and completion scripts
      for topic_folder ($DOTFILES/*) if [ -d $topic_folder ]; then
        fpath=($topic_folder $fpath)
      fi

      # source zsh specific config files
      for file in ''${(M)config_files:#*/shell/*.zsh}; do
        source $file
      done
      config_files=(''${config_files:#*/shell/*.zsh})

      # source everything but the completion files
      for file in ''${config_files:#*/completion.zsh}; do
        source $file
      done
    '';
    initExtra = ''
      # source completion files after autocomplete loads
      for file in ''${(M)config_files:#*/completion.zsh}; do
        source $file
      done

      unset config_files

      # syntax highlighting
      source "$(zsh-syntax-highlighting-share)/zsh-syntax-highlighting.zsh"

      # evaluate .envrc files
      eval "$(direnv hook zsh)"
    '';
  };

  # Git
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
}
