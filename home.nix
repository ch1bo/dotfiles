{ config, pkgs, ... }:
let
  shellAliases = {
    l = "ls -CF";
    ll = "ls -alhF";
    la = "ls -A";
    ls = "ls --color=auto";
    grep = "grep --color=auto";
    fgrep = "fgrep --color=auto";
    egrep = "egrep --color=auto";
    vi = "vim";
    cert = "openssl x509 -noout";
  };
in
{
  programs.home-manager.enable = true;
  home.username = "ch1bo";
  home.homeDirectory = "/home/ch1bo";
  home.stateVersion = "20.09";

  home.packages = [ ];

  # nixpkgs for nix-env etc.
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs/config.nix;
  xdg.configFile."nixpkgs/overlays.nix".source = ./nixpkgs/overlays.nix;

  # shells
  programs.dircolors.enable = true;
  programs.bash = {
    enable = true;
    profileExtra = builtins.readFile ./shell/profile;
    initExtra = builtins.readFile ./shell/bashrc;
    inherit shellAliases;
  };
  programs.zsh = {
    enable = true;
    profileExtra = builtins.readFile ./shell/profile;
    initExtra = builtins.readFile ./shell/zshrc;
    inherit shellAliases;
  };

}
