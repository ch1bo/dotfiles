# Home sweet home.nix

This is the place for my shell (zsh), terminal emulator (urxvt), window manager
(xmonad), editor (emacs) configuration and many more workflow improvements &
shortcuts (git aliases, fzf, z).

I used to symlink configurations from the home directory into this repository to
keep them maintainable, but have now migrated to
[home-manager](https://github.com/nix-community/home-manager). This allows me to
easily update and rollback between generations of my whole user environment, as
well as modularizing host-specific configs.

## Dependencies

* `slock` installed on system-level

## Install

1. Install [nix](https://nixos.org/download.html)
2. `nix-shell --run home-manager switch -f private.nix`

## TODO / Next steps

- [ ] Use home-manager
  - [X] Make self-contained (`home-manager` in `nix-shell`)
  - [X] Migrate using existing config files
  - [ ] Modularize config into proper topics and assemble host modules
  - [ ] Tooling - `nix-direnv`/`lorri` or something like https://github.com/hlissner/dotfiles/blob/master/bin/hey
  - [ ] Make independent of "<nixpkgs>" (using nix flakes)
- [ ] Switch to doom emacs
  - [X] org
  - [X] mail
  - [ ] editor (mc vs. iedit?)
  - [ ] haskell
- [ ] Tray & tune xmobar
- [ ] Improve / new xmonad layouts
- [ ] Theme rofi
- [ ] New terminal emulator
- [ ] Evaluate fish shell or fix zsh completion
- [ ] Clean up with XDG and have proper mime associations
- [ ] Find a better way to screen locking (without dependencies)

## Inspired by

- https://github.com/hlissner/dotfiles
- https://github.com/jkachmar/dotnix
- https://github.com/fzakaria/nix-home
- https://github.com/holman/dotfiles
