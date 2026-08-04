{ pkgs, ... }:
# Declarative BlenderMCP add-on (https://github.com/ahujasid/blender-mcp).
#
# The add-on's register() auto-starts a socket server on localhost:9876
# (blendermcp_auto_start_server defaults to True), which the jailed agent's
# blender-mcp client connects to — see modules/agents.nix. It refuses to run
# under `blender -b`, so a GUI session is required.
#
# Rather than hand-installing addon.py through Blender's preferences, we build a
# BLENDER_USER_SCRIPTS tree containing the add-on plus a startup shim that
# enables it on every launch, and point Blender at it via the session
# environment. No stateful userpref.blend is involved.
#
# Trade-off: BLENDER_USER_SCRIPTS is a single path that replaces the default
# ~/.config/blender/<ver>/scripts location, so add-ons installed manually there
# will not load while this is set.
let
  # Pinned to a commit because upstream publishes no git tags.
  addon = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/ahujasid/blender-mcp/e3ece087adecce4242d4dc3e4db28c33010b51c4/addon.py";
    hash = "sha256-ymlVu1hNeOIp8CCoudcBFECtxulNqwrI4BqyeU2xncA=";
  };

  # Auto-imported by Blender at launch; enables the add-on once the event loop
  # is up so its server can start.
  autoenable = pkgs.writeText "blendermcp_autoenable.py" ''
    import bpy
    import addon_utils


    def _enable():
        try:
            addon_utils.enable("blender_mcp", default_set=True, persistent=True)
        except Exception as exc:
            print("blendermcp autoenable failed:", exc)
        return None


    bpy.app.timers.register(_enable, first_interval=0.1)
  '';

  scripts = pkgs.runCommand "blendermcp-scripts" { } ''
    mkdir -p $out/addons $out/startup
    cp ${addon} $out/addons/blender_mcp.py
    cp ${autoenable} $out/startup/blendermcp_autoenable.py
  '';
in
{
  home.packages = [ pkgs.blender ];

  home.sessionVariables.BLENDER_USER_SCRIPTS = "${scripts}";
}
