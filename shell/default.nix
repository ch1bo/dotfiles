{ config, pkgs, ... }:
let
  shellAliases = {
    ls = "ls --color=auto";
    lt = "lsd --tree";
    ll = "lsd -l";
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
    initExtra = ''
      source ${./colors.zsh};
      source ${./completion.zsh};
      source ${./prompt.zsh};
      source ${./window.zsh};
      source ${./config.zsh};

      # syntax highlighting
      source "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

      # evaluate .envrc files
      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"

      # z for jumping around
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
