# Config for command line fuzzy finder fzf (https://github.com/junegunn/fzf)
export FZF_DEFAULT_COMMAND="fd"
export FZF_DEFAULT_OPTS="--height=40% --reverse"

# Key bindings
FZF_PREFIX=$(dirname $(which fzf))/..
source "${FZF_PREFIX}/share/fzf/key-bindings.zsh"

# z (https://github.com/rupa/z) as source for fzf (only if available)
fzf-z-widget() {
  if type "z" > /dev/null; then
    cd "$(echo $(z -t -l | cut -d' ' -f2- | tr -d ' ' | fzf --tac))"
    zle reset-prompt
  fi
}
zle -N fzf-z-widget
bindkey '^Z' fzf-z-widget
