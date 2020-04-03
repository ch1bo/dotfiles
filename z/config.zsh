# Config/install file for z (https://github.com/rupa/z)

# Add man page if necessary
if [[ ! "$MANPATH" =~ "$DOTFILES/z/man" && -d "$DOTFILES/z/man" ]]; then
  export MANPATH="$MANPATH:$DOTFILES/z/man"
fi

source $DOTFILES/z/z/z.sh
