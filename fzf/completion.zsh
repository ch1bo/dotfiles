# Auto-completion
FZF_PREFIX=$(nix-build -Q '<nixpkgs>' -A fzf)
source "${FZF_PREFIX}/share/fzf/completion.zsh"
