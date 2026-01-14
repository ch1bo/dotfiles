{
  imports = [
    ../../base.nix
    ../../gpg
    ../../home-modules/ssh
  ];

  # TODO: man pages are broken?
  manual.manpages.enable = false;
}
