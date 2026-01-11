{ config, pkgs, ... }:
let
  shellAliases = {
    grep = "grep --color=auto";
    fgrep = "fgrep --color=auto";
    egrep = "egrep --color=auto";
    vi = "vim";
    cert = "openssl x509 -noout";
  };
in
{
  programs.bash = {
    enable = true;
    inherit shellAliases;
    initExtra = builtins.readFile ./bashrc;
  };
  programs.zsh = {
    enable = true;
    inherit shellAliases;
    history = {
      size = 10000000;
      save = 10000000;
      share = false;
    };
    autosuggestion.enable = true;
    initContent = ''
      source ${./colors.zsh};
      source ${./completion.zsh};
      source ${./prompt.zsh};
      source ${./window.zsh};
      source ${./config.zsh};

      # syntax highlighting
      source "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

      # evaluate .envrc files
      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"

      # Stop here if we don't have z/zsh-history set up
      if [[ ! -d ${config.dotfiles}/data ]]; then
        echo "Missing ${config.dotfiles}/data, zsh history and z unavailable; create it and sync via syncthing"
        exit
      fi

      # History backup
      HISTFILE="${config.dotfiles}/data/zsh-history"
      if [[ $(wc -l $HISTFILE | awk '{print $1}') -gt $(wc -l $HISTFILE.bkp | awk '{print $1}' || 0) ]]; then
        cp $HISTFILE $HISTFILE.bkp
      fi

      # z for jumping around
      _Z_DATA=${config.dotfiles}/data/z
      source ${./z.sh}

      # z as source for fzf
      fzf-z-widget() {
        if type "z" > /dev/null; then
          cd "$(echo $(z -t -l | cut -d' ' -f2- | tr -d ' ' | fzf --tac))"
          zle reset-prompt
        fi
      }
      zle -N fzf-z-widget
      bindkey '^Z' fzf-z-widget
    '';
  };

  # Colorize ls
  programs.dircolors.enable = true;
  programs.lsd.enable = true;

  # Load directory specific variables. I mostly 'use_nix' to get
  # project-specific tools from a shell.nix in scope.
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # Fuzzy find everything
  programs.fzf = {
    enable = true;
    defaultCommand = "fd";
    defaultOptions = [ "--height=40%" "--reverse" ];
  };

  home.packages = [
    pkgs.fd # A simple, fast and user-friendly alternative to 'find'
  ];
}
