-- #+AUTHOR: Calin Capitanu
-- #+TITLE: XMONAD Config
-- #+DATE: 26-Jul-2020
--  ____               _ _                 
--  / __ \ __ __ _ _ __(_) |_ __ _ _ _ _  _ 
-- / / _` / _/ _` | '_ \ |  _/ _` | ' \ || |
-- \ \__,_\__\__,_| .__/_|\__\__,_|_||_\_,_|
--  \____/        |_|
-- DISCLAIMER: A lot of the code has been borrowed from DistroTube, all credit for that part go to Derek Taylor form DistroTube.

-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, nextWS, prevWS)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.PhysicalScreens
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Maybe (fromJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.OnPropertyChange
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

    -- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"   -- Sets default terminal

myBrowser :: String
myBrowser = "google-chrome-stable"
-- myBrowser = myTerminal ++ " -e lynx " -- Sets lynx as browser for tree select

myEditor :: String
myEditor = "emacsclient -c -a emacs "  -- Sets emacs as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColor :: String
myNormColor   = "#292d3e"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#bbc5ff"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
          setWMName "LG3D"
          spawn "xmodmap /home/calin/.Xmodmap"
          spawn "xbindkeys --poll-rc &"
          spawnOnce "DISPLAY=\":0\" picom -b"
--          spawnOnce "qjackctl -s &"
          spawnOnce "feh --bg-scale /home/calin/pictures/2.jpg --bg-scale /home/calin/pictures/3.jpg --bg-fill /home/calin/pictures/1.jpg"
          spawnOnce "systemctl start --now --user imwheel"
          spawnOnce "/home/calin/.config/scripts/xrandrfix.sh &"
          spawnOnce "xsetroot -cursor_name left_ptr &"
          spawnOnce "nvidia-settings -load-config-only"   
--          spawnOnce "/home/calin/.config/scripts/devour/devour.sh synergy"
          spawnOnce "spotify"
          spawnOnce "slack"
          spawnOnce "liquidctl -n 0 set led color fixed ff0000"
          spawn "xset s off -dpms"
          spawn "xset s 0 0"





myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x29,0x2d,0x3e) -- lowest inactive bg
                  (0x29,0x2d,0x3e) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x29,0x2d,0x3e) -- active fg


mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "mocp" spawnMocp findMocp manageMocp
                ]
  where
    spawnTerm  = myTerminal ++ " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnMocp  = myTerminal ++ " -n mocp 'mocp'"
    findMocp   = resource =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ mySpacing' 8
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ mySpacing' 4
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ mySpacing' 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=11"
                      , activeColor         = "#292d3e"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#292d3e"
                      , inactiveBorderColor = "#292d3e"
                      , activeTextColor     = "#ffffff"
                      , inactiveTextColor   = "#d0d0d0"
                      }
                  
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Sans:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               -- I've commented out the layouts I don't use.
               myDefaultLayout =     tall
                                 -- ||| magnify
                                 -- ||| noBorders monocle
                                 -- ||| floats
                                 ||| grid
                                 ||| noBorders tabs
                                 -- ||| spirals
                                 -- ||| threeCol
                                 -- ||| threeRow
                                 


myWorkspaces = [" Alderaan ", " Bespin ", " Coruscant ", " Dagobah ", " Endor ", " Felucia ", " Geonosis ", " Hoth ", " Iridonia ", " Jakku ", " Kamino ", " Mustafar "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

replaceSymbol :: Integer -> String
replaceSymbol 11      = "minus"
replaceSymbol 10      = "0"
replaceSymbol 12     = "equal"
replaceSymbol x       =  show x


clickable ws = "<action=xdotool key super+"++replaceSymbol i++">"++ws++"</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices

  
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [
       className =? "Spotify"     --> doShift ( myWorkspaces !! 11 )
     , className =? "ir-Engine"     --> doShift ( myWorkspaces !! 5 )
     , className =? "com-ir22-booksrec-Engine"     --> doShift ( myWorkspaces !! 5 )
     , className =? "Qemu-system-arm"     --> doShift ( myWorkspaces !! 6 )
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 6 )
     , className =? "QjackCtl"     --> doShift ( myWorkspaces !! 10 )
     , className =? "Slack"     --> doShift ( myWorkspaces !! 9 )
     , className =? "org-eclipse-jdt-internal-jarinjarloader-JarRsrcLoader"    --> doFloat
     ] <+> namedScratchpadManageHook myScratchPads

myDynHook = composeAll [
  className =? "Spotify" --> doShift ( myWorkspaces !! 11 )
                       ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0
          

myKeys :: [(String, X ())]
myKeys =
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-e", io exitSuccess)                  -- Quits xmonad
        , ("M-<Return>", spawn myTerminal)
        , ("M-S-l", spawn "i3lock -i /home/calin/.config/lockscreen.png -p win -f")
        , ("M-S-q", kill1)                           -- Kill the currently focused client
        , ("M-S-t", withFocused $ windows . W.sink) -- Push floating window back to tile
        , ("M-m", windows W.focusMaster)     -- Move focus to the master window
        , ("M-j", windows W.focusDown)       -- Move focus to the next window
        , ("M-S-<Left>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<Right>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws
        , ("M-<Left>", windows W.focusUp)       -- Move focus to the next window
        , ("M-k", windows W.focusUp)         -- Move focus to the prev window
        , ("M-<Right>", windows W.focusDown)         -- Move focus to the prev window
        , ("M-S-<Space>", windows W.swapMaster)    -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)      -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)        -- Swap focused window with prev window
        , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
        , ("M-<Backspace>", promote)         -- Moves focused window to master, others maintain order
        , ("M1-S-<Tab>", rotSlavesDown)      -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)         -- Rotate all the windows in the current stack
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws
        , ("M-S-m", spawn "emacsclient -nc") -- start emacs
        , ("M-S-s", spawn "spotify")
        , ("M-S-d", spawn "discord")
          -- , ("M-d", spawn "dmenu_run -i -p 'Arch Linux' -fn 'Ubuntu Mono:bold:pixelsize=20'")
        , ("M-d", spawn "rofi -combi-modi window,drun,ssh -show combi -icon-theme 'Papirus' -show-icons")
        , ("M-S-b", spawn "google-chrome-stable")
        , ("M-S-n", spawn "qutebrowser")
        , ("M-S-g", spawn "guitarix")
        , ("M-S-p", spawn "/home/calin/.config/scripts/rraudio.sh")
        , ("M-S-h", spawn (myTerminal ++ " -e htop")) 
        , ("M-S-f", spawn (myTerminal ++ " -e ranger")) 
    -- Multimedia Keys
        , ("<XF86AudioPlay>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ("<XF86AudioPause>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ("<XF86AudioPrev>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
        , ("<XF86AudioNext>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
        , ("<XF86AudioMute>",   spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")  -- Bug prevents it from toggling correctly in 12.04.
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ("<XF86MonBrightnessUp>", spawn "blight set +20%")
        , ("<XF86MonBrightnessDown>", spawn "blight set -20%")
        , ("<Print>", spawn "scrotd 0")
        , ("<button-1>", spawn "alacritty")
        , ("<mouse-14>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ("<mouse-15>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ("M-S-<Print>", spawn "maim -s --format=png /dev/stdout | xclip -selection clipboard -t image/png -i")
        , ("M-o", nextScreen)  -- Switch focus to next monitor
        , ("M-i", prevScreen)  -- Switch focus to next monitor
        , ("M-[", prevWS)
        , ("M-]", nextWS)
        , ("M-0", windows $ W.greedyView (myWorkspaces !! 9))
        , ("M-S-0", windows $ W.shift (myWorkspaces !! 9))
        , ("M--", windows $ W.greedyView (myWorkspaces !! 10))
        , ("M-S--", windows $ W.shift (myWorkspaces !! 10))
        , ("M-=", windows $ W.greedyView (myWorkspaces !! 11))
        , ("M-S-=", windows $ W.shift (myWorkspaces !! 11))
--        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        ]
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))



--myMouseBindings  =
 --   [
  --    ((0, 14), nextWS)
 --   ,((0, 15), prevWS)
 --   ,((0, 18), (\_ -> spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%" ))
  --  ,((0, 19), (\_ -> spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%" ))
   -- ,((0, 17), (\_ -> spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause" ))
    --,((0, 15), (\_ -> spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next" ))
    --,((0, 16), (\_ -> spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous" ))
    --]

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/calin/.config/xmobar/xmobarrc0"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/calin/.config/xmobar/xmobarrc1"
  xmproc2 <- spawnPipe "xmobar -x 2 /home/calin/.config/xmobar/xmobarrc2"
  xmonad $ ewmh $ docks $ def
        { manageHook = (isDialog --> doF W.shiftMaster <+> doF W.swapDown) <+> ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        -- , handleEventHook    = dynamicPropertyChange "WM_NAME" myDynHook
        --                        <+> serverModeEventHookCmd
        --                        <+> serverModeEventHook
        --                        <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
        --                        <+> docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
          { ppOutput = \x -> hPutStrLn xmproc0 x   >> hPutStrLn xmproc1 x    >> hPutStrLn xmproc2 x
          , ppCurrent = xmobarColor "#aa1313" "" . wrap "[" "]" -- Current workspace in xmobar
          , ppVisible = xmobarColor "#aa1313" "" . clickable               -- Visible but not current workspace
          , ppHidden = xmobarColor "#994599" "" . wrap "*" "" . clickable   -- Hidden workspaces in xmobar
          , ppHiddenNoWindows = xmobarColor "#797979" "" . clickable        -- Hidden workspaces (no windows)
          , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
          , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"                     -- Separators in xmobar
          , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
          , ppExtras  = [windowCount]                           -- # of windows current workspace
          , ppOrder  = \(ws:l:t:ex) -> [ws]
          }
        }
        `additionalKeysP` myKeys
        --`additionalMouseBindings` myMouseBindings
