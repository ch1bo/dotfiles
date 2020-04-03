### NPM completion
COMP_WORDBREAKS=${COMP_WORDBREAKS/=/}
COMP_WORDBREAKS=${COMP_WORDBREAKS/@/}
export COMP_WORDBREAKS

_npm_completion() {
  local si=$IFS
  compadd -- $(COMP_CWORD=$((CURRENT-1)) \
               COMP_LINE=$BUFFER \
               COMP_POINT=0 \
               npm completion -- "${words[@]}" \
               2>/dev/null)
  IFS=$si
}
compdef _npm_completion npm

### Grunt completion
function _grunt_completion() {
  local completions
  # The currently-being-completed word.
  local cur="${words[@]}"
  # The current grunt version, available tasks, options, etc.
  local gruntinfo="$(grunt --version --verbose 2>/dev/null)"
  # Options and tasks.
  local opts="$(echo "$gruntinfo" | awk '/Available options: / {$1=$2=""; print $0}')"
  local compls="$(echo "$gruntinfo" | awk '/Available tasks: / {$1=$2=""; print $0}')"
  # Only add -- or - options if the user has started typing -
  [[ "$cur" == -* ]] && compls="$compls $opts"
  # Trim whitespace.
  compls=$(echo "$compls" | sed -e 's/^ *//g' -e 's/ *$//g')
  # Turn compls into an array to of completions.
  completions=(${=compls})
  # Tell complete what stuff to show.
  compadd -- $completions
}
compdef _grunt_completion grunt
