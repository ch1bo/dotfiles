{ pkgs ? import <nixpkgs> { }, ... }:
pkgs.libratbag.overrideAttrs (oldAttrs: {
  patches = [ ./hidpp20.patch ];
  postInstall = ''
    cp ${./logitech-g502-x-plus-wireless.device} $out/share/libratbag/logitech-g502-x-plus-wireless.device
  '';
})
