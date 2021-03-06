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

Using `home-manager` and the user profile to install things from `nixpkgs`
usually works quite well, but applications with strong ties into the system are
not as straight-forward to set up. The following list contains programs I
usually require to be on the "system-level":

* `slock` - requires root via setuid, maybe a wrapper similar to NixOS helps?
* `nautilus` - uses
  [gvfs](https://wiki.archlinux.org/index.php/File_manager_functionality#Mounting)
  for mounting file systems, which seems not to work (out of the box) when using
  from `nixpkgs`
* `dconf` - to configure GTK etc.
* `pcscd` / `opensc` - for smartcard support.

NOTE: For NixOS this are essentially the required parts of a host's
`configuration.nix`.

## Install

### On non-NixOS

1. Install [nix](https://nixos.org/download.html)
2. `nix-shell --run home-manager switch -f hosts/<host>/home.nix`

### On NixOS

1. Follow [installation instructions](https://nixos.org/manual/nixos/stable/index.html#sec-installation-installing) until `nixos-install`
2. Checkout this repo into `/etc/nixos/dotfiles` and symlink `configuration.nix`
   and `hardware-configuration.nix` with one from `hosts/`.
3. Perform `nixos-install` and reboot.
4. From a vTTY, (optional) move `dotfiles` to `~/.dotfiles` and re-symlink
   `configuration.nix` and `hardware-configuration.nix`, and initialize home
   directory using `nix-shell --run 'home-manager -f hosts/<host>/home.nix switch'`.

### First steps at home

- Prepare doom emacs using `doom sync` and `all-the-icons-install-fonts` (in `emacs`).
- Authenticate nextcloud and sync `keepass` and `org` into `~/sync`.
- Add mail credentials to secrets service and initialize:
  ```
  secret-tool store --label='Mail ncoding.at' \
    port 465 \
    host mail.ncoding.at \
    user sebastian.nagel@ncoding.at
  mu init -m ~/mail --my-address=sebastian.nagel@ncoding.at
  mu index
  ```
- Fetch yubikey and `gpg --receive key C906659A && gpg --card-status` (and `chmod 700 ~/.gnupg`).
- Sign in to Firefox sync.

## TODO / Next steps

- [ ] Modularize home-manager config into proper topics and assemble host modules
- [ ] Make independent of "<nixpkgs>" (using nix flakes)
- [ ] Add tooling to "swap" dotfiles like https://github.com/hlissner/dotfiles/blob/master/bin/hey
- [/] Tray & tune xmobar
- [ ] Improve / new xmonad layouts
- [ ] Theme rofi
- [ ] New terminal emulator
- [ ] Evaluate fish shell or fix zsh completion (+ starship prompt?)
- [ ] Clean up with XDG and have proper mime associations
- [ ] Get rid of the dependencies (above)
- [ ] Set firefox settings / search engines declaratively

## Inspired by

- https://github.com/hlissner/dotfiles
- https://github.com/jkachmar/dotnix
- https://github.com/fzakaria/nix-home
- https://github.com/holman/dotfiles
