{ config, pkgs, ... }:
{
  # TODO use options / mkIf instead?
  # Include any of the ./account-xxx.nix modules

  home.packages = [
    pkgs.gnome3.libsecret # secret-tool
  ];

  accounts.email.maildirBasePath = "mail";

  programs.offlineimap.enable = true;
  programs.mu.enable = true;
}
