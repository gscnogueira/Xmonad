-- Author: Gabriel S. C. Nogueira
-- email : gab.nog94@gmail.com
-- github: github.com/nosgueira

import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces

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

import XMonad.Util.EZConfig
import XMonad.Util.Loggers


main :: IO()
main = xmonad
       . withEasySB mySB defToggleStrutsKey
       . ewmhFullscreen
       . ewmh
       . (`additionalKeysP` aKeys) 
       . (`removeKeysP` rKeys)
       $ myConfig

-------------------------------------------------------------
----------------------GENEREAL-CONFIG------------------------
-------------------------------------------------------------

myConfig = def { modMask  = mod4Mask
                 , terminal = "st"
                 , layoutHook = myLayout
                 , manageHook = myManageHook 
                 , normalBorderColor = "#54595e"
                 , focusedBorderColor = "#51afef"
                 }

-------------------------------------------------------------
----------------------LAYOUTS--------------------------------
-------------------------------------------------------------

myLayout = smartBorders . mkToggle (single NBFULL)
           $ tiled ||| mtiled ||| Full ||| threeCol
  where
    threeCol = rename "Three-Col" $  ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    mtiled   = rename "Mirror-Tall" $ Mirror tiled 
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100
    rename s = renamed [Replace s]


-------------------------------------------------------------
----------------------WINDOW-MANAGEMENT----------------------
-------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myWorkspaces =  map show [1..9]

-------------------------------------------------------------
----------------------KEY-BINDINGS---------------------------
-------------------------------------------------------------
rKeys :: [String] -- Keys do be removed from the default configuration
rKeys = [
  "M-<Space>",
    "M-q",
    "M-S-c"
  ]

aKeys :: [(String, X ())] -- Keys do be added to the default configuration
aKeys = [("M-w"       , spawn "eval $(get_browser)"),
         ("M-e"       , spawn "emacsclient -c -n"),
         ("<Print>"   , spawn "flameshot gui"),
         ("M-S-l"     , spawn "slock"),
         ("M-S-x"     , spawn "poweroff"),
         ("M-<F11>"   , spawn "amixer -D pulse sset Master 1%+"),
         ("M-<F10"    , spawn "amixer -D pulse sset Master 1%-"),
         ("M-<F9>"    , spawn "amixer -D pulse sset Master toggle"),
         ("M-F"       , spawn "pcmanfm"),
         ("M-<Tab>"   , toggleWS),
         ("M-]"       , nextScreen),
         ("M-["       , prevScreen),
         ("M-S-]"     , shiftNextScreen),
         ("M-S-["     , shiftPrevScreen),
         ("M-s"       , swapNextScreen),
         ("M-<U>"     , spawn "xbacklight -inc 1"),
         ("M-<D>"     , spawn "xbacklight -dec 1"),
         ("M-<Space>" , sendMessage $ Toggle NBFULL),
         ("M-r"       , spawn "xmonad --recompile; xmonad --restart"),
         ("M-q"       , kill),
         ("M-0"       , moveTo Next emptyWS),
         ("M-m"       , sendMessage $ JumpToLayout "Tall"),
         ("M-t"       , sendMessage $ JumpToLayout "ThreeCol")
        ]
        ++
        [("M-C-"++(show k), windows $ swapWithCurrent i) | (i, k) <- zip myWorkspaces [1 ..]]

-------------------------------------------------------------
----------------------STATUS-BAR-----------------------------
-------------------------------------------------------------

myXmobarPP :: PP
myXmobarPP = def { ppSep =  gray " | " 
                 , ppCurrent = red . (xmobarBorder "Bottom" "" 2 )
                 , ppVisible = orange
                 , ppTitle =  purple . shorten 50 
                 , ppLayout =  green . shorten 60    -- Title of active layout in xmobar
                 }

  where red    = xmobarColor "#ff6c6b" ""
        orange = xmobarColor "#ECBE7B" ""
        cyan   = xmobarColor "#46D9FF" ""
        gray   = xmobarColor "#54595e" ""
        purple = xmobarColor "#d499e5" ""
        green   = xmobarColor "#98be65" ""





mySB = (statusBarProp "xmobar" (pure myXmobarPP))
