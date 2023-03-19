import XMonad

import XMonad.Actions.CycleWS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances



import XMonad.Util.EZConfig
import XMonad.Util.Loggers


myXmobarPP :: PP
myXmobarPP = def {ppCurrent = xmobarColor "black" "white"}


main :: IO()
main = xmonad
       . ewmhFullscreen
       . ewmh
       . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
       $ myConfig

-------------------------------------------------------------
----------------------LAYOUTS--------------------------------
-------------------------------------------------------------

myLayout = mkToggle (single NBFULL)
           $ tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100

-------------------------------------------------------------
----------------------WINDOW-MANAGEMENT----------------------
-------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myWorkspaces =  ["\57351", "\61574", "\61729", "\61564",
                 "\61465", "\62744", "\61448", "\61441", "\62409"]

-------------------------------------------------------------
----------------------KEY-BINDINGS---------------------------
-------------------------------------------------------------
rKeys :: [String] -- Keys do be removed from the default configuration
rKeys = [
  "M-<Space>",
    "M-q",
    "M-S-c"
  ]

aKeys :: [String] -- Keys do be added to the default configuration
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
         ("M-q"       , kill)
        ]

myConfig = (`additionalKeysP` aKeys) . 
           (`removeKeysP` rKeys)
           $ def { modMask  = mod4Mask
                 , terminal = "st"
                 , layoutHook = myLayout
                 , manageHook = myManageHook 
                 , normalBorderColor = "#54595e"
                 , focusedBorderColor = "#51afef"
                 }
