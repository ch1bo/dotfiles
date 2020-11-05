{ pkgs ? import <nixpkgs> {} }:

{
  # slock for arch linux
  slockArch = pkgs.slock.overrideAttrs (oldAttrs: rec {
    # Use 'nobody' group
    preBuild = ''
      sed -i 's|static const char \*group = "nogroup";|static const char *group = "nobody";|' config.def.h
    '';
  });
}
