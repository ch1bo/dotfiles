# TODO Create a garmin connectiq sdk package instead
{ config, lib, ... }:
let
  current-sdk-cfg = ~/.Garmin/ConnectIQ/current-sdk.cfg;
in
{
  config = lib.mkIf (lib.pathIsRegularFile current-sdk-cfg) {
    home.sessionPath = [
      "${builtins.readFile current-sdk-cfg}/bin"
    ];
  };
}
