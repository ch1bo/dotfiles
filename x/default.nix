{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.xbindkeys
    pkgs.xorg.xrandr
    pkgs.xorg.xrdb
    pkgs.xorg.xset
  ];

  xsession = {
    enable = true;
    initExtra = ''
      # TODO Gnome keyring as SSH agent
      if [ $(which gnome-keyring-daemon 2> /dev/null) ]; then
        eval $(gnome-keyring-daemon --start)
        export SSH_AUTH_SOCK
      fi

      # TODO Start the settings daemon on ubuntu
      if [ $(which unity-settings-daemon 2> /dev/null) ]; then
        unity-settings-daemon &
        sleep 1 # Wait to override settings properly
        cat $HOME/.config/dconf/gnome.conf | dconf load /
      fi

      # No beeps
      xset -b

      # Screensaver 5min, monitor suspend 6min, monitor off 7min
      xset dpms 0 360 420
      xset s blank
      xset s 300

      # Faster keyboard typematic delay and rate
      # TODO this resets when docking or un/plugging keyboards
      xset r rate 200 60

      # TODO use autorandr or grobi
      xrandr --dpi 120
      # Dual head @ work
      #xrandr --output eDP1 --off --output DP1 --output DP2 --right-of DP1

      # Load custom keybindings
      xbindkeys

      # TODO use xresources module?
      # Load colors and settings
      xrdb -load ${./xresources}

      # Desktop wallpaper
      feh --bg-scale ${./matterhorn2.jpg}
    '';
  };

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad/xmonad.hs;
  };

  home.file.".xbindkeysrc".source = ./xbindkeysrc;
  home.file.".xmobarrc" = {
    source = ./xmobarrc;
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
    lockCmd = "/usr/bin/slock";
  };
}
