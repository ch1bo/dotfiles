{ config, lib, pkgs, ... }:
{
  # TODO use options / mkIf instead? also, how to deduplicate with emacs config?
  # Include any of the ./account-xxx.nix modules

  home.packages = [
    pkgs.libsecret # secret-tool
  ];

  accounts.email.maildirBasePath = "mail";

  programs.mu.enable = true;
  programs.msmtp.enable = true;
  programs.offlineimap.enable = true;
}
