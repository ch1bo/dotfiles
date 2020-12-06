{
  imports = [
    ../../common.nix
    ../../mail/account-ncoding.nix
  ];

  home.username = "ch1bo";
  wifi = "wlp58s0";

  # Use host drivers if prepared in $HOME/.nix-opengl-driver, analogous to
  # NixOS's /run/opengl-driver
  #
  # To prepare:
  # - mkdir -p ~/.nix-opengl-driver/lib/dri
  # - cd ~/.nix-opengl-driver/lib/dri; for x in /usr/lib/dri/*; do ln -s $x; done
  # - cd ~/.nix-opengl-driver/lib; for x in /usr/lib/libGL*; do ln -s $x; done
  # - ...a lot of libs... until succeeds:
  # - LD_DEBUG=libs LIBGL_DEBUG=verbose glxinfo -B
  # TODO: only wrap relevant programs like this (as this is a lot of impure/host libs)??
  # TODO: if yes, use nixGL wrappers instead?
  home.sessionVariables = {
    LIBGL_DRIVERS_PATH = "$HOME/.nix-opengl-driver/lib/dri";
    LD_LIBRARY_PATH = "$HOME/.nix-opengl-driver/lib";
  };
}
