# Home sweet home.nix

These are my dotfiles. There are many like it, but these are mine. My dotfiles
are my best friend. They are my life. I must master them as I must master my
life. My dotfiles, without me, are useless. Without my dotfiles, I am useless...
and so on.

This is the place for my shell (zsh), terminal emulator (urxvt), window manager
(xmonad), editor (emacs) configuration and many more workflow improvements &
shortcuts (git aliases, fzf, z).

I used to symlink configurations from the home directory into this repository to
keep them maintainable, but have now migrated to
[home-manager](https://github.com/nix-community/home-manager). This allows me to
easily update and rollback between generations of my whole user environment.

## Install

As I am not (yet) using any `NixOS` systems, [nix](https://nixos.org) needs to
be installed first:

``` sh
curl -L https://nixos.org/nix/install | sh
```

Then, [home-manager](https://github.com/nix-community/home-manager):

``` sh
nix-channel --add https://github.com/nix-community/home-manager/archive/release-20.09.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install
```

And, for now, symlink the default path to the checked out `home.nix`:

``` sh
ln -sf $PWD/home.nix ~/.config/nixpkgs/home.nix
home-manager switch
```

## Disclaimer

No guarantee that this repo will always work for you. I do use this as *my*
dotfiles, so there's a good chance I may break something if I forget to make a
check for a dependency.

## TODO / Next steps

- [ ] Use home-manager
- [ ] Switch to doom emacs
  - [X] org
  - [ ] editor (text, mc/iedit)
  - [ ] mail
  - [ ] haskell
- [ ] Tray & tune xmobar
- [ ] Improve / new xmonad layouts
- [ ] Theme rofi
- [ ] Different terminal emulator
- [ ] Evaluate fish shell
