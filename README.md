# Home sweet ~/

These are my dotfiles. There are many like it, but these are mine. My dotfiles are my best friend. They are my life. I must master them as I must master my life. My dotfiles, without me, are useless. Without my dotfiles, I am useless... and so on.

This is the place for my shell (zsh), terminal emulator (urxvt), window manager (xmonad), editor (emacs) configuration and many more workflow improvements & shortcuts (git aliases, fzf, z).

## Install

Run this:

```sh
git clone https://github.com/ch1bo/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
./bootstrap
```

This will symlink any file or directory suffixed with `.symlink` from your dotfiles to your home directory (see below).

## Topics & symlinking

The first level of directories are 'topics'. They are used to group relevant configuration files, shell scripts, path modifications, autocompletions etc. and are typically referred to from `bin/` and the rc files in the `shell` topics - zsh in my instance.

When running `boostrap`, any file or directory with the `.symlink` suffix is to be symlinked from your home directory. The mechanism does preserve the path below the topic folder and adds the name-giving dot in the home directory. For example my emacs xmonad config `dotfiles/xmonad/xmonad.symlink` is symlinked from `~/.xmonad` while for my gnome gtk3 config `dotfiles/gnome/config/gtk-3.0.symlink` the symlink `~/config/gtk-3.0` is created.

## Conventions

There's a few special files in the hierarchy.

- **bin/**: is added to `$PATH` and should refer globally available scripts and executables.
- **topic/\*.zsh**: Are sourced from `.zshrc` and used to configure the zsh environment.
- **topic/path.zsh**: Are sourced first from `.zshrc` and is expected to setup `$PATH` or similar.
- **topic/completion.zsh**: Are sourced last from `.zshrc` and is expected to setup autocomplete.
- **topic/\*\*/\*.symlink**: Any files ending in `.symlink` get symlinked
  from your `$HOME` at the corresponding path when executing `bootstrap`.

## Disclaimer

No guarantee that this repo will always work for you. I do use this as *my*
dotfiles, so there's a good chance I may break something if I forget to make a
check for a dependency.

## TODO
[ ] List prerequisites and use `nix` to install them
[ ] Detect orphan links by dumping list of paths in `bootstrap` and check them on next run
