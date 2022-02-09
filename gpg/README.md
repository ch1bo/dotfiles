# NixOS Live USB for GPG

Taken from https://github.com/drduh/YubiKey-Guide#nixos

## Build and create a USB

```
nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=nixos-gpg.nix
sudo dd if=result/iso/<name>.iso of=/dev/<usb-stick> status=progress
```

## Build and run as VM

TODO: boot not complete? no GUI, only console right now

```
nix-build '<nixpkgs/nixos>' -A vm -I nixos-config=nixos-gpg.nix
./result/bin/run-nixos-vm
```
