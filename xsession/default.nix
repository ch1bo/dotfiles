{ config, lib, pkgs, types, ... }:

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
        # TODO Use keepassxc as libsecret and SSH agent
        if [ $(which gnome-keyring-daemon 2> /dev/null) ]; then
          eval $(gnome-keyring-daemon --start)
          export SSH_AUTH_SOCK
        fi

        # No beeps
        xset -b

        # Screen saver (blank) after 3min, lock 2min later
        xset s blank
        xset s 180 120

        # Faster keyboard typematic delay and rate
        # TODO this resets when docking or un/plugging keyboards
        xset r rate 200 60

        # TODO use autorandr or grobi
        xrandr --dpi 120
        # Dual head @ work
        #xrandr --output eDP1 --off --output DP1 --output DP2 --right-of DP1

        # Load custom keybindings
        ${pkgs.xbindkeys}/bin/xbindkeys

        # TODO use xresources module?
        # Load colors and settings
        xrdb -load ${./xresources}
        xrdb -merge $HOME/.Xresources

        # Desktop wallpaper
        ${pkgs.feh}/bin/feh --bg-scale ${./matterhorn2.jpg}
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
      xssLockExtraOptions =
        let dimScreenScript = pkgs.writeScript "dim-screen"
          (builtins.readFile "${pkgs.xss-lock}/share/doc/xss-lock/dim-screen.sh");
        in [ "-n ${dimScreenScript}" ];
    };

    # launched by xmonad
    programs.rofi.enable = true;

    home.packages = [
      pkgs.trayer # launched by xmonad
      pkgs.xmobar # launched by xmonad
      pkgs.xorg.setxkbmap # TODO keyboard module and finally add short cuts
      pkgs.xorg.xrandr # manage monitors # TODO use autorandr/grobi?
      pkgs.xorg.xrdb # manipulate xresources
      pkgs.xorg.xset # configure xorg
    ];

  };
}
