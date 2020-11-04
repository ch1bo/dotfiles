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
  programs.dircolors.enable = true;
  programs.bash = {
    enable = true;
    inherit shellAliases;
    profileExtra = builtins.readFile ./profile;
    initExtra = builtins.readFile ./bashrc;
  };
  programs.zsh = {
    enable = true;
    inherit shellAliases;
    history = {
      size = 10000000;
      save = 10000000;
    };
    profileExtra = builtins.readFile ./profile;
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
}
