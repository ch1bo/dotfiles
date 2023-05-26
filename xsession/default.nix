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
            eDP-1 = "00ffffffffffff0006af936600000000001e0104a51d127803f795a6534aa0260d505400000001010101010101010101010101010101fa3c80b870b0244010103e001eb2100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231333355414e30312e32200a0066";
            DP-1 = "00ffffffffffff00410c3f09657100002b1e0104a55021783a10e5ad5048a526135054bfef00d1c0b3009500818081c0316845686168e77c70a0d0a0295030203a001d4e3100001a000000ff00554b3032303433303239303239000000fc0050484c203334365031430a2020000000fd0030641ea03c010a202020202020018502031af14d0103051404131f12021190595a23090707830100004ed470a0d0a0465030403a001d4e3100001c507800a0a038354030203a001d4e3100001ed8590060a3382840a0103a101d4e3100001aef51b87062a0355080b83a001d4e3100001c0000000000000000000000000000000000000000000000000000000000fa";
          };
        in
        {
          docked = {
            inherit fingerprint;
            config = {
              DP-1 = {
                primary = true;
                crtc = 1;
                mode = "3440x1440";
                position = "0x0";
                rate = "100.00";
              };
              eDP-1 = {
                crtc = 1;
                mode = "1920x1200";
                position = "3440x0";
                rate = "60.00";
              };
            };
          };

          mobile = {
            inherit fingerprint;
            config = {
              eDP-1 = {
                primary = true;
                crtc = 0;
                mode = "1920x1200";
                position = "0x0";
                rate = "60.00";
              };
            };
          };
        };
    };
  };
}
