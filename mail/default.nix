{ config, lib, pkgs, pkgs-2311, ... }:
{
  # TODO use options / mkIf instead? also, how to deduplicate with emacs config?
  # Include any of the ./account-xxx.nix modules

  home.packages = [
    pkgs.libsecret # secret-tool
    pkgs.mailctl # for oauth2 mail access
  ];

  accounts.email.maildirBasePath = "mail";

  programs.mu.enable = true;
  # Use mu 1.10 as doom-emacs has still problems with latest 1.12
  programs.mu.package = pkgs-2311.mu;

  programs.msmtp.enable = true;
  programs.offlineimap = {
    enable = true;
    pythonFile = builtins.readFile ./get_settings.py;
  };

  xdg.configFile."mailctl/config.yaml".source = ./mailctl-config.yaml;
  xdg.configFile."mailctl/services.yaml".source = ./mailctl-services.yaml;
}
