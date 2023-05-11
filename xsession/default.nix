{ config, lib, pkgs, types, ... }:
let
  setBackground = "${pkgs.feh}/bin/feh --bg-max ${./matterhorn-wide.jpg}";
  setKeyboardRate = "xset r rate 200 60";
in
{

  options.wifi = lib.mkOption {
    type = lib.types.str;
    example = "wlp58s0";
    description = "Wifi device name";
  };

  config = {
    xsession = {
      enable = true;
      initExtra = ''
        # No beeps
        xset -b

        # Screen saver (blank) after 3min, lock 2min later
        xset s blank
        xset s 180 120

        # Faster keyboard typematic delay and rate
        ${setKeyboardRate}

        # Load custom keybindings
        ${pkgs.xbindkeys}/bin/xbindkeys

        # TODO use xresources module?
        # Load colors and settings
        xrdb -load ${./xresources}
        xrdb -merge $HOME/.Xresources

        # Initial dpi setting
        xrandr --dpi 120

        # Desktop wallpaper
        ${setBackground}
      '';
    };

    xsession.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad/xmonad.hs;
    };

    home.file.".xbindkeysrc".source = ./xbindkeysrc;
    home.file.".xmobarrc" = {
      source = pkgs.substituteAll {
        src = ./xmobarrc.hs;
        wifi = "${config.wifi}";
      };
      onChange = ''
        if [[ -v DISPLAY ]] ; then
          $DRY_RUN_CMD ${config.xsession.windowManager.command} --restart
        fi
      '';
    };

    # Use system-level screen locker as either
    # * setuid flags are required, or
    # * pam-based lockers do not play well.
    services.screen-locker = {
      enable = true;
      lockCmd = lib.mkDefault "/usr/bin/slock";
      xss-lock.extraOptions =
        let dimScreenScript = pkgs.writeScript "dim-screen"
          (builtins.readFile "${pkgs.xss-lock}/share/doc/xss-lock/dim-screen.sh");
        in [ "-n ${dimScreenScript}" ];
    };

    # launched by xmonad
    programs.rofi.enable = true;

    # access pulse incl. volume control from tray
    services.pasystray.enable = true;

    home.packages = [
      pkgs.brightnessctl # used by xbindkeys
      pkgs.trayer # launched by xmonad
      pkgs.xmobar # launched by xmonad
      pkgs.xorg.xrandr # manage monitors
      pkgs.xorg.xrdb # manipulate xresources
      pkgs.xorg.xset # configure xorg
      pkgs.maim # for taking screenshots, used by xbindkeys
      pkgs.xclip # for taking screenshots, used by xbindkeys
    ];
  };
}
