Config { font =         "xft:FiraCode Nerd Font Mono:size=12:antialias=true"
       , bgColor =      "#21242b"
       , fgColor =      "#bbc2cf"
       , position =     Top

       -- layout
       , sepChar =  "%"   -- delineator between plugin names and straight text
       , alignSep = "}{"  -- separator between left-right alignment
       , template = "%StdinReader% }{ %multicpu% %multicoretemp% %memory% %dynnetwork% %battery% %bright% %kbd% %date% %trayerpad%"

       -- general behavior
       , lowerOnStart =     True    -- send to bottom of window stack on start
       , hideOnStart =      False   -- start with window unmapped (hidden)
       , allDesktops =      True    -- show on all desktops
       , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
       --, pickBroadest =     False   -- choose widest display (multi-monitor)
       , persistent =       True    -- enable/disable hiding (True = disabled)

       -- plugins see http://projects.haskell.org/xmobar/#system-monitor-plugins.
       , commands =
         [ Run StdinReader
         -- TODO broken? always "Updating..."
         -- , Run Wireless "@wifi@" [ "--template", "<essid>" ] 10
         , Run DynNetwork     [ "--template" , " <tx>kB/s  <rx>kB/s"
                              , "--Low"      , "1000"       -- units: kB/s
                              , "--High"     , "5000"       -- units: kB/s
                              , "--low"      , "#99c794"
                              , "--normal"   , "#5fb3b3"
                              , "--high"     , "#ec5f67"
                              ] 10
         , Run MultiCpu       [ "--template" , "碌 <total>%"
                              , "--Low"      , "50"         -- units: %
                              , "--High"     , "85"         -- units: %
                              , "--low"      , "#99c794"
                              , "--normal"   , "#5fb3b3"
                              , "--high"     , "#ec5f67"
                              ] 10
         , Run MultiCoreTemp  [ "--template" , " <max>°C"
                              , "--Low"      , "50"        -- units: °C
                              , "--High"     , "80"        -- units: °C
                              , "--low"      , "#99c794"
                              , "--normal"   , "#5fb3b3"
                              , "--high"     , "#ec5f67"
                              , "--"
                              , "--hwmon-path", "/sys/class/hwmon/hwmon2/"
                              ] 50
         , Run Memory         [ "--template" ," <usedratio>%"
                              , "--Low"      , "30"        -- units: %
                              , "--High"     , "90"        -- units: %
                              , "--low"      , "#99c794"
                              , "--normal"   , "#5fb3b3"
                              , "--high"     , "#ec5f67"
                              ] 10
         , Run Battery        [ "--template" , "<acstatus>"
                              , "--Low"      , "20"        -- units: %
                              , "--High"     , "60"        -- units: %
                              , "--low"      , "#ec5f67"
                              , "--normal"   , "#5fb3b3"
                              , "--high"     , "#99c794"
                              , "--" -- battery specific options
                              -- AC "on" status
                              , "-O" , "<fc=#33B5E1></fc>"
                              -- AC "idle" status
                              , "-i" , "<fc=#5fb3b3></fc>"
                              -- discharging status
                              , "-o" , " <left>%"
                              , "--highs", "" -- higher than -H
                              , "--mediums", "" -- between -H and -L
                              , "--lows", "" -- lower than -L
                              ] 50
         --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
         , Run Date           "%a %F %T" "date" 10
         , Run Kbd            [ ("us", "US")
                              , ("de", "DE")
                              ]
         , Run Mail [("﯍", "~/mail/ncoding.at/INBOX")] "mail"
         , Run Com "trayerpad" [] "trayerpad" 10
         ]
       }
