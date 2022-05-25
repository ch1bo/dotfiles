{
  imports = [
    ../../base.nix
    ../../gpg
  ];

  home.username = "ch1bo";

  # Only gpg binary, without agent etc.
  programs.gpg.enable = true;
}
