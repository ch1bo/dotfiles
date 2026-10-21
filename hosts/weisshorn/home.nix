{
  imports = [
    ../../base.nix
    ../../gpg
    ../../home-modules/ssh
  ];

  shell.dataDir = "/data";
}
