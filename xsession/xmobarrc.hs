Config { font =         "FiraCode Nerd Font Mono 14"
       , bgColor =      "#21242b"
       , fgColor =      "#bbc2cf"
       , position =     TopH 24

       -- layout
       , sepChar =  "%"   -- delineator between plugin names and straight text
       , alignSep = "}{"  -- separator between left-right alignment
       , template = "%StdinReader% }{ %dynnetwork% %multicpu% %k10temp% %memory% %battery% %kbd% %date% %trayerpad%"

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
         , Run K10Temp        "0000:00:18.3"
                              [ "--template" , " <Tctl>°C"
                              , "--Low"      , "40"        -- units: °C
                              , "--High"     , "70"        -- units: °C
                              , "--low"      , "#99c794"
                              , "--normal"   , "#5fb3b3"
                              , "--high"     , "#ec5f67"
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
         , Run Date           "%a %Y-%m-%d %H:%M" "date" 10
         , Run Kbd            [ ("us", "US")
                              , ("de", "DE")
                              ]
         , Run Com "trayerpad" [] "trayerpad" 10
         ]
       }
