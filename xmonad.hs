-- Author: Gabriel S. C. Nogueira
-- email : gab.nog94@gmail.com
-- github: github.com/nosgueira

import Data.Monoid
import System.Exit
import System.Process
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GroupNavigation
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Promote
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CycleWindows
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.MultiToggle 
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "st"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 3

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = [" 爵 "," \63592 "," \61728 "," \63616 "," \61485 "," ﭧ "," \63411 "," 拾 "," \61504 "]


xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
    where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myClickableWorkspaces :: [String]
myClickableWorkspaces = clickable . (map xmobarEscape)
        $ ["1","2","3","4","5","6","7","8","9"]
    where 
clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#434C5E"
myFocusedBorderColor = "#8fbcbb"


------------------------------------------------------------------------
--------------------------MY-FUNCTIONS----------------------------------
------------------------------------------------------------------------
us="setxkbmap -model abnt -layout us -variant intl -option ctrl:nocaps"
br="setxkbmap -layout br -option ctrl:nocaps"
toggleKbd :: String  -> String
toggleKbd c | br == c = us
toggleKbd _ = br

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run -p 'dmenu:' -hp chromium,firefox,telegram")

    -- close focused window
    , ((modm , xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_period ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_space   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), promote)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((mod1Mask,           xK_j     ), sendMessage MirrorShrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((mod1Mask,           xK_k     ), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_i ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_d), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((mod1Mask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    -- My Keybindings
    ++
        -- Louch Programs
    [ ((modm,                       xK_b ), spawn "brave" ), 
        ((modm,                     xK_f), spawn "firefox"),
        ((modm,                     xK_e), spawn "emacs"),
        ((0,                        xK_Print), spawn "flameshot gui"),
        ((modm,                     xK_bracketleft), nextScreen),
        ((modm,                     xK_bracketright), prevScreen),
        ((mod1Mask,                 xK_Tab), nextWS),
        ((mod1Mask .|. shiftMask,   xK_Tab), prevWS),
        ((modm,                     xK_Tab), toggleWS),
        ((modm,                     xK_s), swapNextScreen),
        ((modm .|. shiftMask,       xK_bracketright), shiftNextScreen),
        ((modm .|. shiftMask,       xK_bracketleft), shiftPrevScreen),
        ((modm .|. shiftMask,       xK_l), spawn "slock"),
        ((mod1Mask .|. shiftMask,   xK_q), spawn "poweroff"),
        ((modm ,                    xK_F11), spawn "amixer -D pulse sset Master 1%+"),
        ((modm ,                    xK_F10), spawn "amixer -D pulse sset Master 1%-"),
        ((modm ,                    xK_F9), spawn "amixer -D pulse sset Master toggle"),
        ((modm,                     xK_c), spawn "~/.local/bin/show_configs")
       ,((modm,                     xK_a), spawn "pcmanfm")
       ,((modm,                     xK_w), spawn "sxiv -t ~/Images/wallpapers/WallpaperDesktop")
       ,((modm,                     xK_z), rotFocusedUp)
       ,((modm .|. shiftMask,       xK_z), rotFocusedDown)
       ,((modm,                     xK_space), sendMessage $ MT.Toggle NBFULL)
    ]
    ++
    [((modm .|. controlMask, k), windows $ swapWithCurrent i)
    | (i, k) <- zip myClickableWorkspaces [xK_1 ..]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

myLayout =  avoidStruts $  mkToggle ( NBFULL ?? NOBORDERS ?? EOT) $  ( tiled ||| Mirror tiled |||noBorders Full )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled =  named "Tiled" $ spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True $ ResizableTall nmaster delta ratio []

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "MEGAsync"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , title  =? "Salvar arquivo" --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.6)
    , title  =? "Abrir arquivo" --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.6)
    , resource  =? "kdesktop"       --> doIgnore,
    isFullscreen--> ( doF W.focusDown <+> doFullFloat )]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
        spawnOnce "trayer --transparent true --alpha 0 --tint 0x2E3440 --edge top --align right --height 23 --width 7 --monitor 1 --expand true &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
--
main = do 
    xmproc0<-spawnPipe "xmobar -x 1 /home/gabriel/.config/xmobar/xmobarNoTrayer.config"
    xmproc1<-spawnPipe "xmobar -x 0 /home/gabriel/.config/xmobar/xmobar.config"
    xmonad $docks $ewmh  $ def {
        -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myClickableWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        -- hooks, layouts
        layoutHook         = lessBorders OnlyScreenFloat $  myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = myEventHook <+> fullscreenEventHook,
        logHook            = dynamicLogWithPP $ xmobarPP{
                              ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
                            , ppCurrent = xmobarColor "#BF616A" "" . wrap "[" "]" -- Current workspace in xmobar
                            , ppVisible = xmobarColor "#D08770" ""                -- Visible but not current workspace
                            , ppHidden = xmobarColor "#88C0D0" ""                 -- Hidden workspaces in xmobar
                            , ppHiddenNoWindows = xmobarColor "#4C566A" ""        -- Hidden workspaces (no windows)
                            , ppTitle = xmobarColor "#B48EAD" "" . shorten 35     -- Title of active window in xmobar
                            , ppLayout = xmobarColor "#EBCB8B" "" . shorten 60    -- Title of active layout in xmobar
                            , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                            , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                            , ppSep =  "<fc=#666666> <fn=1>|</fn></fc>"           -- Separators in xmobar
                           },
        startupHook        = myStartupHook
    }



