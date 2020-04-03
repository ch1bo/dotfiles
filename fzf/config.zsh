# Config for command line fuzzy finder fzf (https://github.com/junegunn/fzf)
export FZF_DEFAULT_COMMAND="ag -l -g ''"
export FZF_DEFAULT_OPTS="--height=40% --reverse"
local fzfroot="$DOTFILES/fzf/fzf"

# Man path
if [[ ! "$MANPATH" =~ "$fzfroot/man" && -d "$fzfroot/man" ]]; then
  export MANPATH="$MANPATH:$fzfroot/man"
fi

# Auto-completion
[[ $- == *i* ]] && source "$fzfroot/shell/completion.zsh" 2> /dev/null

# Key bindings
source "$fzfroot/shell/key-bindings.zsh"
bindkey '^G' fzf-cd-widget

# z (https://github.com/rupa/z) as source for fzf (only if available)
fzf-z-widget() {
  if type "z" > /dev/null; then
    cd "$(echo $(z -t -l | cut -d' ' -f2- | tr -d ' ' | fzf --tac))"
    zle reset-prompt
  fi
}
zle -N fzf-z-widget
bindkey '^Z' fzf-z-widget
