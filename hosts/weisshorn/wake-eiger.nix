{ config, lib, pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.writeShellScriptBin
      "wake-eiger"
      "${pkgs.wakeonlan}/bin/wakeonlan 30:c5:99:57:ee:4b")
  ];
}
