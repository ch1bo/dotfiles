autoload colors && colors

# Context: user@hostname
prompt_context() {
  local context
  context+=%{$fg_bold[black]%}[%{$reset_color%}
  context+=%{$fg[magenta]%}%n%{$reset_color%}
  if [[ -n "$SSH_CONNECTION" ]]; then
    context=$context%{$fg[yellow]%}@%m%{$reset_color%}
  fi
  context+=%{$fg_bold[black]%}]%{$reset_color%}
  echo "$context"
}

# Dir: current working directory
prompt_dir() {
  local dir
  dir+=%{$fg_bold[black]%}[%{$reset_color%}
  if [[ -n $PROMPT_COLLAPSE ]]; then
    dir+=%{$fg[blue]%}%3(c:.../:)%2c%{$reset_color%}
  else
    dir+=%{$fg[blue]%}%~%{$reset_color%}
  fi
  dir+=%{$fg_bold[black]%}]%{$reset_color%}
  echo $dir
}

# Git: current HEAD tag, branch or commit
prompt_git() {
  local head=$(git symbolic-ref HEAD 2> /dev/null ||
               git describe HEAD --all --exact-match 2> /dev/null ||
               git log -n 1 --format="%h" 2> /dev/null)
  # Remove prefixed tags/, refs/, remotes/, heads/
  head=${head#tags/}
  head=${head#refs/heads/}
  head=${head#remotes/}
  if [[ -n $head ]]; then
    local str=""
    str+="%{$fg_bold[black]%}[%{$reset_color%}"
    if [[ -n $PROMPT_COLLAPSE ]]; then
      str+="%{$fg[cyan]%}⌥%{$reset_color%}"
    else
      str+="%{$fg[cyan]%}⌥ $head%{$reset_color%}"
    fi
    str+="%{$fg_bold[black]%}]%{$reset_color%}"
    echo $str
  fi
}

# Docker: current DOCKER_MACHINE_NAME
prompt_docker() {
  if [[ -n $DOCKER_MACHINE_NAME ]]; then
    local str=""
    str+="%{$fg_bold[black]%}[%{$reset_color%}"
    str+="%{$fg[yellow]%}$DOCKER_MACHINE_NAME%{$reset_color%}"
    str+="%{$fg_bold[black]%}]%{$reset_color%}"
    echo $str
  fi
}

# AWS: current AWS_PROFILE
prompt_aws() {
  if [[ -n $AWS_PROFILE ]]; then
    local str=""
    str+="%{$fg_bold[black]%}[%{$reset_color%}"
    str+="%{$fg[yellow]%}$AWS_PROFILE%{$reset_color%}"
    str+="%{$fg_bold[black]%}]%{$reset_color%}"
    echo $str
  fi
}

# Status:
# - was there an error
# - am I root
# - are there background jobs?
prompt_status() {
  local symbols=()
  [[ $RETVAL -ne 0 ]] && [[ $RETVAL -ne 130 ]] && symbols+="%{$fg[red]%}✘"
  [[ $UID -eq 0 ]] && symbols+="%{$fg_bold[red]%}⚡"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{$fg[cyan]%}⚙"
  if [[ $ELAPSED -gt 10000 ]]; then
     symbols+="%{$fg[red]%}󰁫$((${ELAPSED}/1000))s"
  elif [[ $ELAPSED -gt 500 ]]; then
     symbols+="%{$fg[yellow]%}󰁫${ELAPSED}ms"
  fi
  if test ${#symbols[@]} -gt 0; then
    local str=""
    str+="%{$fg_bold[black]%}[%{$reset_color%}"
    str+="$symbols%{$reset_color%}"
    str+="%{$fg_bold[black]%}]%{$reset_color%}"
    echo $str
  fi
}

collapse_prompt() {
  if [[ -n $PROMPT_COLLAPSE ]]; then
    unset PROMPT_COLLAPSE
  else
    export PROMPT_COLLAPSE=true
  fi
}

set_prompt() {
  if [[ -n $PROMPT_COLLAPSE ]]; then
    export PROMPT="$(prompt_dir)$(prompt_git)$(prompt_status)%{$fg[cyan]%} λ %{$reset_color%}"
  else
    export PROMPT="$(prompt_context)$(prompt_dir)$(prompt_git)$(prompt_docker)$(prompt_aws)$(prompt_status)%{$fg[cyan]%} λ %{$reset_color%}"
  fi
  # SPROMPT='zsh: correct %F{red}%R%f to %F{green}%r%f [nyae]? '
}

preexec() {
  export TIMER=$(($(date +%s%0N)/1000000))
  title "$1"
}

precmd() {
  RETVAL=$?
  title "zsh %~"
  if [ $TIMER ]; then
    local now=$(($(date +%s%0N)/1000000))
    ELAPSED=$(($now-$TIMER))
    unset TIMER
  fi
  set_prompt
  unset ELAPSED
}

chpwd() {
  unset RETVAL
}
