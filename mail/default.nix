{ config, lib, pkgs, ... }:
{
  # TODO use options / mkIf instead? also, how to deduplicate with emacs config?
  # Include any of the ./account-xxx.nix modules

  home.packages = [
    pkgs.libsecret # secret-tool
    pkgs.oama # for oauth2 mail access
  ];

  accounts.email.maildirBasePath = "mail";

  programs.mu.enable = true;
  programs.emacs.extraPackages = epkgs: [ epkgs.mu4e ];
  programs.msmtp.enable = true;
  programs.offlineimap = {
    enable = true;
    pythonFile = builtins.readFile ./get_settings.py;
  };

  xdg.configFile."oama/config.yaml".source = ./oama-config.yaml;
}
