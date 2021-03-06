{
  imports = [
    ../../base.nix
  ];

  home.username = "ch1bo";

  # Only gpg binary, without agent etc.
  programs.gpg.enable = true;
}
