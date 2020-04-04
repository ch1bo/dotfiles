# Enable use of sudo with aliases
alias sudo="nocorrect sudo "

# enable color support of ls and also add handy aliases
if [ -x $(which dircolors) ]; then
  [ -r ~/.dircolors ] && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls="ls --color=auto"

  alias grep="grep --color=auto"
  alias fgrep="fgrep --color=auto"
  alias egrep="egrep --color=auto"
fi

# some more ls aliases
alias ll="ls -alhF"
alias la="ls -A"
alias l="ls -CF"
alias vi="vim"
alias skype="apulse32 skype"
alias cert="openssl x509 -noout"
