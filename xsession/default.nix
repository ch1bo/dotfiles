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

        # Initial dpi setting, later autorandr manages things
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
      xssLockExtraOptions =
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
      pkgs.xorg.setxkbmap # TODO keyboard module and finally add short cuts
      pkgs.xorg.xrandr # manage monitors
      pkgs.xorg.xrdb # manipulate xresources
      pkgs.xorg.xset # configure xorg
    ];

    # Automatic screen setup
    programs.autorandr = {
      enable = true;

      hooks.postswitch = {
        # HACK Wait 3s until KVM switched, use udev instead?
        "reset-keyboard-rate" = "sleep 3; ${setKeyboardRate}";
        "reset-background" = setBackground;
      };

      profiles =
        let
          fingerprint = {
            DP-1 = "00ffffffffffff00410c3f09657100002b1e0104a55021783a10e5ad5048a526135054bfef00d1c0b3009500818081c0316845686168e77c70a0d0a0295030203a001d4e3100001a000000ff00554b3032303433303239303239000000fc0050484c203334365031430a2020000000fd0030641ea03c010a202020202020018502031af14d0103051404131f12021190595a23090707830100004ed470a0d0a0465030403a001d4e3100001c507800a0a038354030203a001d4e3100001ed8590060a3382840a0103a101d4e3100001aef51b87062a0355080b83a001d4e3100001c0000000000000000000000000000000000000000000000000000000000fa";
            eDP-1 = "00ffffffffffff0026cf360500000000001d0104a51d11780285b0945753932722505400000001010101010101010101010101010101363680a07038204018303c0026a510000019000000000000000000000000000000000000000000fe00496e666f566973696f6e0a2020000000fe004d3133334e574634205241200a002e";
          };
        in
        {
          docked = {
            inherit fingerprint;
            config = {
              eDP-1.enable = false;
              DP-1 = {
                primary = true;
                crtc = 1;
                mode = "3440x1440";
                position = "0x0";
                rate = "100.00";
              };
            };
          };

          mobile = {
            inherit fingerprint;
            config = {
              eDP-1 = {
                primary = true;
                crtc = 0;
                mode = "1920x1080";
                position = "0x0";
                rate = "60.00";
              };
            };
          };
        };
    };
  };
}
