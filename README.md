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

As I have not (yet) made the jump to [NixOS](https://nixos.org), I am using
`nixpkgs` (via `home-manager`) as an "after-market" package manager on an
arbitrary host linux distribution to declaratively manage my home enviroment.
This usually works quite well, but applications with strong ties into the system
are not as straight-forward to set up. The following list contains programs I
usually require to be on the "system-level":

* `slock` - requires root via setuid, maybe a wrapper similar to NixOS helps?
* `nautilus` - uses
  [gvfs](https://wiki.archlinux.org/index.php/File_manager_functionality#Mounting)
  for mounting file systems, which seems not to work (out of the box) when using
  from `nixpkgs`
* `dconf` - to configure GTK etc.

## Install

### On non-NixOS

1. Install [nix](https://nixos.org/download.html)
2. `nix-shell --run home-manager switch -f hosts/<host>/home.nix`
3. Prepare doom emacs using `doom sync`
4. TODO Install secrets

### On NixOS

1. Follow [installation instructions](https://nixos.org/manual/nixos/stable/index.html#sec-installation-installing)
2. But symlink `/etc/nixos/configuration.nix` with one from `hosts/<host>/configuration.nix` before `nixos-install`
3. Prepare doom emacs using `doom sync`
4. TODO Install secrets

## TODO / Next steps

- [ ] Modularize home-manager config into proper topics and assemble host modules
- [ ] Make independent of "<nixpkgs>" (using nix flakes)
- [ ] Add tooling to "swap" dotfiles like https://github.com/hlissner/dotfiles/blob/master/bin/hey
- [ ] Tray & tune xmobar
- [ ] Improve / new xmonad layouts
- [ ] Theme rofi
- [ ] New terminal emulator
- [ ] Evaluate fish shell or fix zsh completion
- [ ] Clean up with XDG and have proper mime associations
- [ ] Get rid of the dependencies (above)

## Inspired by

- https://github.com/hlissner/dotfiles
- https://github.com/jkachmar/dotnix
- https://github.com/fzakaria/nix-home
- https://github.com/holman/dotfiles
