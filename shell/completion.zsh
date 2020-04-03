# also use approximate completer
zstyle ':completion:*' completer _complete _ignored _approximate

# ignore .. and . when completing
zstyle ':completion:*' ignore-parents parent pwd

# matches case insensitive for lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# pasting with tabs doesn't perform completion
zstyle ':completion:*' insert-tab pending

# arrow-key driven interface
zstyle ':completion:*' menu select

# correct up to 2 errors
zstyle ':completion:*' max-errors 2

# use ls colors in list
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# use a more verbose completion
zstyle ':completion:*' verbose true

# completion prompts
# zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
# zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s

