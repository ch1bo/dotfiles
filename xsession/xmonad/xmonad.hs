{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import System.Exit (exitSuccess)
import System.IO (Handle)
import XMonad hiding (config)
import XMonad.Actions.Navigation2D (screenGo, switchLayer, windowGo, windowSwap, withNavigation2DConfig)
import XMonad.Hooks.DynamicLog (PP (..), filterOutWsPP, shorten, statusBar, wrap, xmobarColor)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageHelpers (doFloatDep)
import XMonad.Hooks.StatusBar (StatusBarConfig, defToggleStrutsKey, statusBarProp, withEasySB)
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.LayoutModifier (ModifiedLayout (..))
import XMonad.Layout.Maximize (maximize, maximizeRestore)
import XMonad.Layout.MultiToggle (Toggle (..), Transformer (..), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (SMARTBORDERS))
import XMonad.Layout.ResizableTile (MirrorResize (..), ResizableTall (..))
import XMonad.Layout.Spacing (Border (..), Spacing (..), spacingRaw)
import XMonad.StackSet (RationalRect (..), focusDown, greedyView, shift, sink, swapDown)
import XMonad.Util.NamedScratchpad (NamedScratchpad (..), customFloating, namedScratchpadAction, namedScratchpadManageHook, scratchpadWorkspaceTag)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Types (Direction2D (..))

main :: IO ()
main = tray >> xmonad config

config =
    def
        { modMask = mod4Mask -- Super as modifier
        , terminal = "urxvt"
        , focusFollowsMouse = True -- Focus on mouse enter
        , clickJustFocuses = False -- Click 'into' window
        , normalBorderColor = "#21242b"
        , focusedBorderColor = "#51afef"
        , borderWidth = 3
        , keys = keyBindings
        , layoutHook = layouts
        , manageHook = manageHooks
        }
        & ewmh
        & withNavigation2DConfig def
        & withEasySB xmobar defToggleStrutsKey -- mod + b

xmobar :: StatusBarConfig
xmobar = statusBarProp "xmobar" (pure prettyStatus)
  where
    prettyStatus =
        def
            { ppCurrent = xmobarColor "#51afef" "" . wrap "[" "]"
            , ppTitle = xmobarColor "#51afef" "" . shorten 40
            , ppVisible = wrap "(" ")"
            }
            & filterOutWsPP [scratchpadWorkspaceTag]

-- TODO(SN): process stays alive when recompiling/restarting xmonad
tray :: IO Handle
tray =
    spawnPipe $
        unwords
            [ "trayer"
            , "--edge top"
            , "--align right"
            , "--widthtype request"
            , "--expand true"
            , "--SetDockType true"
            , "--SetPartialStrut true"
            , "--transparent true"
            , "--alpha 0"
            , "--tint 0x21242b"
            , "--height 24"
            ]

keyBindings conf@XConfig{XMonad.modMask = modMask} =
    Map.fromList $
        -- Quit xmonad
        [ ((modMask .|. shiftMask, xK_q), io exitSuccess)
        , -- Restart xmonad

            ( (modMask .|. shiftMask, xK_r)
            , spawn "xmonad --recompile; xmonad --restart"
            )
        , -- Launch terminal
          ((modMask, xK_Return), spawn $ terminal conf)
        , -- Launch programs with rofi
          ((modMask, xK_p), spawn "rofi -show run")
        , -- Switch windows with rofi
          ((modMask, xK_o), spawn "rofi -show window")
        , -- Close focused window
          ((modMask, xK_q), kill)
        , -- Rotate through the available layout algorithms
          ((modMask, xK_space), sendMessage NextLayout)
        , -- Reset the layouts on the current workspace to default
          ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
        , -- Resize viewed windows to the correct size
          ((modMask, xK_n), refresh)
        , -- Focus next/previous window in the stack
          ((modMask, xK_Tab), windows focusDown)
        , ((modMask .|. shiftMask, xK_Tab), windows swapDown)
        , -- Shrink/Expand the master area
          ((modMask, xK_minus), sendMessage Shrink)
        , ((modMask, xK_equal), sendMessage Expand)
        , -- Increment/decremnet the number of windows in the master area
          ((modMask, xK_comma), sendMessage (IncMasterN 1))
        , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
        , -- Push window back into tiling
          ((modMask, xK_t), withFocused $ windows . sink)
        , -- Switch between layers (tiled, floating)
          ((modMask .|. controlMask, xK_Tab), switchLayer)
        , -- Directional navigation of windows
          ((modMask, xK_l), windowGo R False)
        , ((modMask, xK_h), windowGo L False)
        , ((modMask, xK_k), windowGo U False)
        , ((modMask, xK_j), windowGo D False)
        , -- Swap adjacent windows
          ((modMask .|. shiftMask, xK_l), windowSwap R False)
        , ((modMask .|. shiftMask, xK_h), windowSwap L False)
        , ((modMask .|. shiftMask, xK_k), windowSwap U False)
        , ((modMask .|. shiftMask, xK_j), windowSwap D False)
        , -- Resize windows TODO(SN): switch keys when (not) mirrored layout
          ((modMask .|. controlMask, xK_l), sendMessage MirrorShrink)
        , ((modMask .|. controlMask, xK_h), sendMessage MirrorExpand)
        , ((modMask .|. controlMask, xK_j), sendMessage Expand)
        , ((modMask .|. controlMask, xK_k), sendMessage Shrink)
        , -- Directional navigation of screens
          ((modMask, xK_grave), screenGo L True)
        , ((modMask, xK_BackSpace), screenGo R True)
        , -- Toggle maximize
          ((modMask, xK_m), withFocused (sendMessage . maximizeRestore))
        , -- Toggle smart borders on layout
          ((modMask, xK_z), sendMessage $ Toggle SMARTBORDERS)
        , -- Toggle gaps and spacing on layout
          ((modMask, xK_x), sendMessage $ Toggle EXPLODE)
        , -- Show/hide scratchpad
          ((modMask, xK_s), namedScratchpadAction scratchpads "terminal")
        , -- Launch browser
          ((modMask, xK_w), spawn "firefox")
        , -- Lock screen
          ((controlMask .|. mod1Mask, xK_l), spawn "loginctl lock-session")
        , -- Switch language
          ((modMask, xK_d), spawn "setxkbmap de")
        , ((modMask, xK_u), spawn "setxkbmap us")
        ]
            ++
            --
            -- mod-[1..9], Switch to workspace N
            -- mod-shift-[1..9], Move client to workspace N
            --
            [ ((m .|. modMask, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(greedyView, 0), (shift, shiftMask)]
            ]

-- Layout transformer to apply gap and spacing to layouts via MultiToggle
data EXPLODE = EXPLODE
    deriving (Read, Show, Eq, Typeable)

instance Transformer EXPLODE Window where
    transform EXPLODE x k =
        k
            (gaps [(U, 10), (R, 10), (D, 10), (L, 10)] $ spacing 10 x)
            (\(ModifiedLayout _ (ModifiedLayout _ x')) -> x')

spacing :: Int -> l a -> ModifiedLayout Spacing l a
spacing i = spacingRaw False (uniformBorder 0) False (uniformBorder i') True
  where
    i' = fromIntegral i
    uniformBorder x = Border x x x x

layouts =
    mkToggle (single SMARTBORDERS)
        . mkToggle (single EXPLODE)
        $ tiled
            ||| Mirror tiled
            ||| Full
  where
    -- Resizable tiling with maximize modifier
    tiled = maximize $ ResizableTall nmaster delta ratio []
    -- The default number of windows in the master pane
    nmaster = 2
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

manageHooks =
    composeAll
        [ className =? "Gimp" --> doFloat
        , (className <&> isPrefixOf ".blueman") --> doFloatTopRight
        , (className <&> flip elem ["Pavucontrol", "Paprefs"]) --> doFloatTopRight
        , teamsNotification
        ]
        <+> namedScratchpadManageHook scratchpads
  where
    teamsNotification =
        title
            =? "Microsoft Teams Notification"
            --> doFloat
            <+> doF focusDown

    doFloatTopRight = doFloatDep $ \(RationalRect _ _ w h) ->
        RationalRect (1 - w) xmobarRationalHeight w 0.5

    -- HACK
    xmobarRationalHeight = 0.02

scratchpads =
    [ NS
        { name = "terminal"
        , cmd = terminal config <> " -name scratchpad"
        , query = resource =? "scratchpad"
        , hook = customFloating (RationalRect 0.10 0.25 0.8 0.5)
        }
    ]
