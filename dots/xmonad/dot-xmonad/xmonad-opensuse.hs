import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Tabbed
import System.IO
import XMonad.Config.Desktop

import qualified Data.Map as M

-- make sure to edit paths to xmobar and .xmobarrc to match your system.
    -- If xmobar is in your $PATH, and its config is in ~/.xmobarrc you don't
    -- need the xmobar path or config file, use: xmproc <- spawnPipe "xmobar"
main = do
    dzen <- spawnPipe myStatusBar
--    conkytop <- spawnPipe myTopBar
--    conkympd <- spawnPipe myMPDBar
--    conkyhdd <- spawnPipe myHDDBar
    xmproc <- spawnPipe "/home/tsureshkumar/.cabal/bin/xmobar /home/tsureshkumar/.xmobarrc"
    xmonad $ gnomeConfig
        { 
          ---manageHook = manageDocks <+> manageHook gnomeConfig <+>
          ---			title =? "foo" --> doShift "2" <+> isFullscreen -?> doFullFloat
          manageHook = composeAll 
                       [
                         manageDocks
                       , manageHook gnomeConfig
                       , title =? "foo" --> doShift "2" 
                       , isFullscreen --> doFullFloat
                       ]
---        , layoutHook =  desktopLayoutModifiers $ avoidStruts  $  layoutHook gnomeConfig
        , layoutHook = desktopLayoutModifiers (Tall 1 0.03 0.5 ||| Full)
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
	, terminal = myterm
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
	, ((mod4Mask, xK_e), spawn "emacs")
        , ((mod4Mask, xK_g), spawn "google-chrome")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
	, ((0                     , 0x1008FF11), spawn "amixer set Master 2%-")
        , ((0                     , 0x1008FF13), spawn "amixer set Master 2%+")
        , ((0                     , 0x1008FF12), spawn "amixer set Master toggle")
	, ((0                     , 0x1008FF03), spawn "xbacklight -dec 10%")
        , ((0                     , 0x1008FF02), spawn "xbacklight -inc 10%")
        ]

-- Color, font and iconpath definitions:
myFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-c-*-*-*"
myIconDir = "/home/and1/.dzen"
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
myPatternColor = "#1f1f1f"
mySeperatorColor = "#555555"

-- myterm = "urxvt --perl-lib ~/.urxvt +sb" 
myterm = "terminator"
-- Statusbar options:
myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w '1300' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myTopBar = "conky -c .conkytop | dzen2 -x '1300' -y '0' -h '16' -w '620' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myMPDBar = "conky -c .conkympd | dzen2 -x '0' -y '1184' -h '16' -w '1600' -ta 'l' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myHDDBar = "conky -c .conkyhdd | dzen2 -x '1600' -y '1184' -h '16' -w '320' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
