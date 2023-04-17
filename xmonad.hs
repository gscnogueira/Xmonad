import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Promote

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing


import XMonad.Util.EZConfig
import XMonad.Util.Loggers

import XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

main :: IO()
main = xmonad
       . withEasySB mySB defToggleStrutsKey
       . ewmhFullscreen
       . ewmh
       . (`additionalKeysP` aKeys) 
       . (`removeKeysP` rKeys)
       $ myConfig

myConfig = def { modMask  = mod4Mask
                 , terminal = "st"
                 , layoutHook = myLayout
                 , manageHook = myManageHook   
                 , normalBorderColor = "#54595e"
                 , focusedBorderColor = "#51afef"
                 }

myLayout = smartBorders . mkToggle (single NBFULL)
           $ tiled ||| mtiled ||| Full ||| threeCol
  where
    threeCol = rename "Three-Col" $  ThreeColMid nmaster delta ratio
    tiled    = rename "Tall" $ gaps $ Tall nmaster delta ratio
    mtiled   = rename "Mirror-Tall" $ Mirror tiled 
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100
    rename s = renamed [Replace s]
    gaps = spacingRaw False (Border gap_size 0 gap_size 0) False (Border 0 gap_size 0 gap_size) False
    gap_size = 5

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat,
    namedScratchpadManageHook scratchpads
    ]

myWorkspaces =  map show [1..9]

scratchpads = [
-- run htop in xterm, find it by title, use default floating window placement
    NS "terminal" "st -t 'NSP'" (title =? "NSP") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))]

rKeys :: [String]
rKeys = ["M-<Space>",
         "M-q",
         "M-S-c",
         "M-<Return>"
        ]

aKeys :: [(String, X ())]
aKeys = [("M-w"         , spawn "eval $(get_browser)"),
         ("M-e"         , spawn "emacsclient -c -n"),
         ("<Print>"     , spawn "flameshot gui"),
         ("M-S-l"       , spawn "slock"),
         ("M-S-x"       , spawn "poweroff"),
         ("M-<F11>"     , spawn "amixer -D pulse sset Master 1%+"),
         ("M-<F10"      , spawn "amixer -D pulse sset Master 1%-"),
         ("M-<F9>"      , spawn "amixer -D pulse sset Master toggle"),
         ("M-f"         , spawn "pcmanfm"),
         ("M-<Tab>"     , toggleWS' ["NSP"]),
         ("M-]"         , nextScreen),
         ("M-["         , prevScreen),
         ("M-S-]"       , shiftNextScreen),
         ("M-S-["       , shiftPrevScreen),
         ("M-s"         , swapNextScreen),
         ("M-<U>"       , spawn "xbacklight -inc 1"),
         ("M-<D>"       , spawn "xbacklight -dec 1"),
         ("M-S-<Space>"   , sendMessage $ Toggle NBFULL),
         ("M-r"         , spawn "killall xmobar; xmonad --recompile && xmonad --restart"),
         ("M-C-d"       , spawn "rofi -show drun -show-icons"),
         ("M-C-s"       , spawn "rofi -show drun -show-icons"),
         ("M-C-w"       , spawn "rofi -show window -show-icons"),
         ("C-<Space>"   , spawn "dunstctl close"),
         ("C-S-<Space>" , spawn "dunstctl close-all"),
         ("M-d h"       , spawn "dunstctl history-pop"),
         ("M-q"         , kill),
         ("M-0"         , moveTo Next emptyWS),
         ("M-a 1"       , sendMessage $ JumpToLayout "Tall"),
         ("M-a 2"       , sendMessage $ JumpToLayout "Mirror-Tall"),
         ("M-a 3"       , sendMessage $ JumpToLayout "Three-Col"),
         ("M-a s"       , sequence_ [toggleScreenSpacingEnabled, toggleWindowSpacingEnabled]),
         ("M-<Return>"  , promote),
         ("M-<Space>"  , namedScratchpadAction scratchpads "terminal" )
        ]
        ++
        [("M-C-"++(show k), windows $ swapWithCurrent i) | (i, k) <- zip myWorkspaces [1 ..]]

mySB = (xmobar_1 <> xmobar_2)
  where xmobar_1 = statusBarProp "xmobar -x 1 ~/.xmonad/xmobar/xmobar_2" (pure myXmobarPP)
        xmobar_2 = statusBarProp "xmobar -x 0 ~/.xmonad/xmobar/xmobar_1" (pure myXmobarPP)

myXmobarPP :: PP
myXmobarPP = def { ppSep     =  gray " | " 
                 , ppCurrent = red . (xmobarBorder "Bottom" "" 3 ) 
                 , ppVisible = orange 
                 , ppHidden =  white . isNSP
                 , ppTitle   = purple . shorten 80 
                 , ppLayout  = green . shorten 60    -- Title of active layout in xmobar
                 , ppOrder = \[ws, l, w] -> [ws, l, w]
                 }

  where red    = xmobarColor "#ff6c6b" ""
        orange = xmobarColor "#ECBE7B" ""
        cyan   = xmobarColor "#46D9FF" ""
        gray   = xmobarColor "#54595e" ""
        white   = xmobarColor "#FFFFFF" ""
        purple = xmobarColor "#d499e5" ""
        green   = xmobarColor "#98be65" ""
        blue   = xmobarColor "#51afef" ""
        isNSP x = if x == "NSP" then "" else x
