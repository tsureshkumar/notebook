{-# LANGUAGE
     DeriveDataTypeable,
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     PatternGuards,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances
     #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}

-- Imports.
import XMonad
import XMonad.Hooks.DynamicLog

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig { modMask = mod4Mask } 


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
