# Home sweet home.nix

This is the place for my shell (zsh), terminal emulator (alacritty), window
manager (xmonad), editor (emacs) configuration and many more workflow
improvements & shortcuts (git aliases, fzf, z).

I used to symlink configurations from the home directory into this repository to
keep them maintainable, but have now migrated to
[home-manager](https://github.com/nix-community/home-manager) on
[NixOS](https://nixos.org) for (eventually) a full declarative experience. This
allows me to easily update and rollback between generations of my whole user
environment, as well as modularizing host-specific configs.

## Install

### On NixOS

1. Follow [installation
   instructions](https://nixos.org/manual/nixos/stable/index.html#sec-installation-installing)
   until `nixos-install`
2. Make sure you have mounted `/`, `/boot`, `/nix` and `/home` into `/mnt`.
3. Do a `nixos-install --flake github:ch1bo/dotfiles?submodules=1#<host>`, set
   passwords using `nixos-enter` and reboot.
4. Rebuild system via `nixos-rebuild --flake ".?submodules=1" switch`

### Not on NixOS

Using `home-manager` and the user profile to install things from `nixpkgs`
usually works quite well, but applications with strong ties into the system are
not as straight-forward to set up. The following list contains programs I
usually require to be on the "system-level":

- `slock` - requires root via setuid, maybe a wrapper similar to NixOS helps?
- `nautilus` - uses
  [gvfs](https://wiki.archlinux.org/index.php/File_manager_functionality#Mounting)
  for mounting file systems, which seems not to work (out of the box) when using
  from `nixpkgs`
- `dconf` - to configure GTK etc.
- `pcscd` / `opensc` - for smartcard support.

NOTE: For NixOS this are essentially the required parts of a host's
`configuration.nix`.

After installing these dependencies, it's just:

1. Install [nix](https://nixos.org/download.html)
2. `nix-shell --run home-manager switch -f hosts/<host>/home.nix`

### First steps at home

- Setup syncthing / nextcloud and sync `keepass` and `org` into `~/sync`.
- Fetch yubikey and `gpg --receive-key 0x59FAA903C906659A && gpg --card-status` (and `chmod 700 ~/.gnupg`).
- Prepare doom emacs using `doom sync` and `all-the-icons-install-fonts` (in `emacs`).
- Add mail credentials to secrets service, initialize and receive mails:
  ```
  secret-tool store --label='Mail ncoding.at' \
    port 465 \
    host mail.ncoding.at \
    user sebastian.nagel@ncoding.at
  mu init -m ~/mail --my-address=sebastian.nagel@ncoding.at
  mu index
  offlineimap -o
  ```
- Sign in to Firefox sync.
- Generate `nix-index`.
- Setup auto-type and minimization in `keepassxc`.

## TODO / Next steps

- [x] Flake-based nixos configurations (no symlinking)
- [x] Make independent of "<nixpkgs>" (using nix flakes)
- [ ] Do auto-upgrades from CI builds (with ways to go back)
- [ ] Modularize into proper topics
  - [x] Re-integrate home config with system configuration.nix
  - [ ] DRY and modularize system configuration properly between systems
- [ ] Declarative syncthing setup
- [ ] Drop dual use support (only nixos)
  - [ ] Get rid of the dependencies (above)
- [ ] Add tooling to "swap" dotfiles like https://github.com/hlissner/dotfiles/blob/master/bin/hey
- [ ] Theme rofi
- [ ] New terminal emulator
- [ ] Clean up with XDG and have proper mime associations
- [ ] Reduce "first steps at home" (above)
- [ ] Set firefox settings / search engines declaratively

## Inspired by

- https://github.com/hlissner/dotfiles
- https://github.com/jkachmar/dotnix
- https://github.com/fzakaria/nix-home
- https://github.com/holman/dotfiles
