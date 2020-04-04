# topic-specific .profile which is sourced by any login shell
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
  . $HOME/.nix-profile/etc/profile.d/nix.sh
fi
# requires: nix-env -i glibc-locales
if [ -e $HOME/.nix-profile/lib/locale/locale-archive ]; then
  export LOCALE_ARCHIVE=$HOME/.nix-profile/lib/locale/locale-archive
fi
