{-# LANGUAGE
     DeriveDataTypeable,
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     PatternGuards,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances,
     TemplateHaskell
     #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}

-- Imports.
import XMonad
import XMonad.Hooks.DynamicLog

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.SetWMName

import XMonad.Layout.Monitor

import XMonad  
import XMonad.Layout.NoBorders  
import XMonad.Layout.PerWorkspace

import XMonad.Layout.LayoutModifier


--tabby bar

import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks

import           System.Taffybar.Hooks.PagerHints (pagerHints)

   

-- The main function. 
-- main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig {
  --startupHook = setWMName "LG3D"
  --, manageHook = manageHook myConfig <+> composeAll myManagementHooks <+> manageMonitor clock
  --, workspaces = myWorkspaces
  --, layoutHook = myLayout
  --}
main = xmonad $ ewmh defaultConfig { handleEventHook =
           handleEventHook defaultConfig <+> fullscreenEventHook }

-- Command to launch the bar.
myBar = "taffybar"
--myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig --= defaultConfig { 
--myConfig = gnomeConfig { 
	modMask = mod4Mask 
	, terminal = "terminator"

	} `additionalKeys`
      [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command -l"), -- lock screen
        ((0                     , 0x1008FF11), spawn "amixer set Master 2%-"),
        ((0                     , 0x1008FF13), spawn "amixer set Master 2%+"),
        ((0                     , 0x1008FFB2), spawn "amixer set Capture toggle"),
        ((0                     , 0x1008FF12), spawn "amixer set Master toggle")
      , ((mod4Mask, xK_u     ), broadcastMessage ToggleMonitor >> refresh)
      ]


{- snippet for xmobar log hook in XMonad -}
-- myLogHook = dynamicLogString myStatusBarPP
--             >>= xmonadPropLog
--             >> updatePointer (Relative 0.5 0.5)
            
-- myStatusBarPP = xmobarPP { ppCurrent = xmobarColor myHighlightColor "" . workspaceClick "[" "]"
--                          , ppUrgent = xmobarColor myUrgentColor "" . workspaceClick "[" "]"
--                          , ppHidden =  workspaceClick " " " " . hideScratchpad
--                          , ppWsSep = ""
--                          , ppVisible = workspaceClick "[" "]"
--                          , ppLayout = layoutClick "" " "
--                          , ppSep = "|"
--                          , ppOrder = \(ws:l:t:r) -> l:ws:t:r
--                          , ppTitle = xmobarColor myHighlightColor "" . wrap " " "" . xmobarStrip
--                          }
--      where hideScratchpad     ws = if ws == "NSP" then "" else ws
--            workspaceClick l r ws = wrap ("<action=wmctrl -s "++wsn ws++">"++l) (r++"</action>") ws
--            layoutClick    l r    = wrap ("<fn=1><action=xdotool key super+space>"++l) (r++"</action></fn>")
--            wsn "0" = "9"
--            wsn "NSP" = "10"
--            wsn ws = show $ (read ws :: Int) - 1

clock = monitor {
      -- Cairo-clock creates 2 windows with the same classname, thus also using title
      prop = ClassName "Cairo-clock" `And` Title "MacSlow's Cairo-Clock"
      -- rectangle 150x150 in lower right corner, assuming 1280x800 resolution
    , rect = Rectangle (1600-150) (900-150) 150 150
      -- avoid flickering
             
    , persistent = True
      -- make the window transparent
    , opacity = 0.2
      -- hide on start
    , visible = True
      -- assign it a name to be able to toggle it independently of others
    , name = "clock"
    }


defaultLayouts = ModifiedLayout clock $ tiled ||| Mirror tiled ||| Full  
  where  
    -- default tiling algorithm partitions the screen into two panes  
    tiled = Tall nmaster delta ratio  
    
    -- The default number of windows in the master pane  
    nmaster = 1  
    
    -- Default proportion of screen occupied by master pane  
    ratio = 2/3  
   
    -- Percent of screen to increment by when resizing panes  
    delta = 3/100  
   
-- Define layout for specific workspaces  
nobordersLayout = noBorders $ Full  
   
-- Put all layouts together  
--myLayout = onWorkspace "7:chat" nobordersLayout $ defaultLayouts
myLayout = defaultLayouts  
   
myWorkspaces = ["1:main","2","3","4","5:media","6:dev","7:chat","8:web", "9:mail"]  
myManagementHooks :: [ManageHook]
myManagementHooks = [
  resource =? "stalonetray" --> doIgnore
  , className =? "emacs" --> doShift "9:mail" -- put emacs in 9th workspace
  , className =? "google-chrome" --> doShift "8:web"
  , className =? "Pidgin" --> doShift "7:chat"
--  , manageDocks
  ]

