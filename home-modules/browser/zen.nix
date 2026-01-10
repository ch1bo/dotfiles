# Using https://github.com/0xc000022070/zen-browser-flake
#
# TODO: extensions and ideally sync setup
#
# TODO: keybindings are stored in zen-keyboard-shortcuts.json, should be able to
# update this with custom overrides similar to places.sqlite is modified.
{ inputs, config, pkgs, lib, ... }:
{
  imports = [
    inputs.zen-browser.homeModules.twilight
  ];

  programs.zen-browser.enable = true;

  programs.zen-browser.policies =
    let
      mkLockedAttrs = builtins.mapAttrs (_: value: {
        Value = value;
        Status = "locked";
      });

      mkPluginUrl = id: "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";

      mkExtensionEntry =
        { id
        , pinned ? false
        ,
        }:
        let
          base = {
            install_url = mkPluginUrl id;
            installation_mode = "force_installed";
          };
        in
        if pinned
        then base // { default_area = "navbar"; }
        else base;

      mkExtensionSettings = builtins.mapAttrs (_: entry:
        if builtins.isAttrs entry
        then entry
        else mkExtensionEntry { id = entry; });
    in
    {
      AutofillAddressEnabled = true;
      AutofillCreditCardEnabled = false;
      DisableAppUpdate = true;
      DisableFeedbackCommands = true;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DontCheckDefaultBrowser = true;
      NoDefaultBookmarks = true;
      OfferToSaveLogins = false;
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
      };
      SanitizeOnShutdown = {
        FormData = true;
        Cache = true;
      };
      # TODO ExtensionSettings = mkExtensionSettings { };
      Preferences = mkLockedAttrs {
        "browser.aboutConfig.showWarning" = false;
        "browser.tabs.warnOnClose" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = true;
        # Disable swipe gestures (Browser:BackOrBackDuplicate, Browser:ForwardOrForwardDuplicate)
        "browser.gesture.swipe.left" = "";
        "browser.gesture.swipe.right" = "";
        "browser.tabs.hoverPreview.enabled" = true;
        "browser.newtabpage.activity-stream.feeds.topsites" = false;
        "browser.topsites.contile.enabled" = false;

        "privacy.resistFingerprinting" = true;
        "privacy.resistFingerprinting.randomization.canvas.use_siphash" = true;
        "privacy.resistFingerprinting.randomization.daily_reset.enabled" = true;
        "privacy.resistFingerprinting.randomization.daily_reset.private.enabled" = true;
        "privacy.resistFingerprinting.block_mozAddonManager" = true;
        "privacy.spoof_english" = 1;

        "privacy.firstparty.isolate" = true;
        "network.cookie.cookieBehavior" = 5;
        "dom.battery.enabled" = false;

        "gfx.webrender.all" = true;
        "network.http.http3.enabled" = true;
        "network.socket.ip_addr_any.disabled" = true; # disallow bind to 0.0.0.0
      };
    };

  programs.zen-browser.profiles.default = rec {
    settings = {
      "zen.workspaces.continue-where-left-off" = true;
      "zen.workspaces.natural-scroll" = true;
      "zen.view.compact.hide-tabbar" = true;
      "zen.view.compact.hide-toolbar" = true;
      "zen.view.compact.animate-sidebar" = true;
      "zen.welcome-screen.seen" = true;
      "zen.urlbar.behavior" = "float";
    };

    spacesForce = true;
    spaces = {
      "Personal" = {
        id = "4d929899-3c7c-44e3-be00-e1e850836b6f";
        icon = "🏡";
        position = 1000;
        theme = {
          type = "gradient";
          colors = [{
            algorithm = "floating";
            type = "explicit-lightness";
            red = 107;
            green = 126;
            blue = 148;
            lightness = 50;
            position = { x = 51; y = 97; };
          }];
          opacity = 0.5;
        };
      };
      "Work" = {
        id = "1aa8cdd7-cf7b-4523-a2aa-20d3f085dfd3";
        icon = "🧑‍💻";
        position = 2000;
        theme = {
          type = "gradient";
          colors = [{
            algorithm = "floating";
            type = "explicit-lightness";
            red = 84;
            green = 140;
            blue = 171;
            lightness = 50;
            position = { x = 68; y = 137; };
          }];
          opacity = 0.5;
        };
      };

    };

    # TODO: these seem not to work?
    pinsForce = true;
    pins = {
      "Clockify" = {
        id = "fbe8aca9-6962-45eb-a099-0e7e18e9f25d";
        workspace = spaces."Work".id;
        url = "https://app.clockify.me/tracker";
        isEssential = true;
        position = 0;
      };
      "GitHub" = {
        id = "f6f117f5-8c5d-42f5-b8db-ded620fc2de2";
        workspace = spaces."Work".id;
        url = "https://github.com/notifications";
        isEssential = true;
        position = 1;
      };
    };

    search = {
      force = true;
      default = "ddg";
      engines =
        let
          nixSnowflakeIcon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
        in
        {
          "Nix Packages" = {
            urls = [
              {
                template = "https://search.nixos.org/packages";
                params = [
                  { name = "type"; value = "packages"; }
                  { name = "channel"; value = "unstable"; }
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }
            ];
            icon = nixSnowflakeIcon;
            definedAliases = [ "p" ];
          };

          "Nix Options" = {
            urls = [
              {
                template = "https://search.nixos.org/options";
                params = [
                  { name = "channel"; value = "unstable"; }
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }
            ];
            icon = nixSnowflakeIcon;
            definedAliases = [ "o" ];
          };

          "Home Manager Options" = {
            urls = [
              {
                template = "https://home-manager-options.extranix.com/";
                params = [
                  { name = "query"; value = "{searchTerms}"; }
                  { name = "release"; value = "master"; }
                ];
              }
            ];
            icon = nixSnowflakeIcon;
            definedAliases = [ "hm" ];
          };

          "Google Maps" = {
            urls = [{
              template = "http://maps.google.com";
              params = [{ name = "q"; value = "{searchTerms}"; }];
            }];
            definedAliases = [ "maps" "gmaps" ];
          };

          "ddg" = {
            urls = [{
              template = "https://duckduckgo.com";
              params = [
                { name = "q"; value = "{searchTerms}"; }
                { name = "origin"; value = "unknown"; }
              ];
            }];
            definedAliases = [ "duck" "ddg" "dck" "dckk" ];
          };

          bing.metaData.hidden = "true";
        };
    };
  };

  # XXX: For some reason I need this installs section for it to work
  home.file.".zen/profiles.ini" = {
    text =
      let
        # Vendored generation code from mkFirefoxModule
        profiles =
          lib.flip lib.mapAttrs' config.programs.zen-browser.profiles
            (_: profile:
              lib.nameValuePair "Profile${toString profile.id}" {
                Name = profile.name;
                Path = profile.path;
                IsRelative = 1;
                Default = if profile.isDefault then 1 else 0;
              }
            ) // {
            General = {
              StartWithLastProfile = 1;
              Version = config.programs.zen-browser.profileVersion;
            };
          };
      in
      lib.generators.toINI { } (profiles // {
        Install166448B1A78F3C9E = {
          Default = "default";
          Locked = 1;
        };
      });
  };

  # Open files with the browser
  xdg.mimeApps =
    let
      associations = builtins.listToAttrs (map
        (name: {
          inherit name;
          value =
            let
              zen-browser = config.programs.zen-browser.package;
            in
            zen-browser.meta.desktopFileName;
        }) [
        "application/x-extension-shtml"
        "application/x-extension-xhtml"
        "application/x-extension-html"
        "application/x-extension-xht"
        "application/x-extension-htm"
        "x-scheme-handler/unknown"
        "x-scheme-handler/mailto"
        "x-scheme-handler/chrome"
        "x-scheme-handler/about"
        "x-scheme-handler/https"
        "x-scheme-handler/http"
        "application/xhtml+xml"
        "application/json"
        "text/plain"
        "text/html"
      ]);
    in
    {
      associations.added = associations;
      defaultApplications = associations;
    };
}
