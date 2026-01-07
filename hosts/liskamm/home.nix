{
  imports = [
    ../../base.nix
    ../../gpg
  ];

  # TODO: man pages are broken?
  manual.manpages.enable = false;

  home.username = "ch1bo";
}
