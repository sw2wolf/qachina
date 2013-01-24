-- Simon Shine's xmonad configuration
-- http://shine.eu.org/ -- http://github.com/sshine/
import System.IO (hPutStrLn)
import Data.List (isInfixOf)

import XMonad
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.ManageDocks -- (manageDocks, ToggleStruts)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.DynamicLog -- (dynamicLogWithPP, xmobarPP, shorten, pp...)
import XMonad.Prompt.Window (windowPromptGoto, windowPromptBring)
import XMonad.Prompt -- (defaultXPConfig, amberXPConfig, ...,amberXPConfig, autoComplete)
import qualified XMonad.StackSet as W

import XMonad.Util.Scratchpad

-- Java swing fix
import XMonad.Hooks.ICCCMFocus (takeTopFocus)
import XMonad.Hooks.SetWMName (setWMName)

----- Start
main = do
  xmobar <- spawnPipe $ xmobar ++ " " ++ xmobarrc
  xmonad $ defaultConfig
    { modMask = mod4Mask
    , workspaces = myWorkspaces
    , normalBorderColor = "#dddddd"
    , focusedBorderColor = "#00ff00"
    , layoutHook = avoidStruts . smartBorders $ myLayout
    , logHook = takeTopFocus >> myLogHook xmobar
    , manageHook = manageDocks <+> myManageHook <+> myManageScratchpadHook
    , startupHook = myStartupHook
    }
    `additionalKeysP` myKeys

  where
    xmobar   = "/home/simon/.cabal/bin/xmobar"
    xmobarrc = "/home/simon/.xmonad/xmobarrc"

-- TODO: Create layout with only two visible windows
myLayout = Full ||| Mirror (Tall 1 (3/100) (1/2))

myManageHook = composeAll
    [ className =? "MPlayer"            --> doFloat
    , title     =? "xmessage"           --> doCenterFloat
    , title     =? "Unlock private key" --> doCenterFloat
    , title     =? "error"              --> doCenterFloat
    , resource  =? "desktop_window"     --> doIgnore
    , resource  =? "kdesktop"           --> doIgnore
    ]

myManageScratchpadHook =
  scratchpadManageHook (W.RationalRect left top width height)
  where
    left   =  2/100
    width  = 96/100
    top    =  5/100
    height = 30/100

myWorkspaces = map show [1..9]

myKeys = [
    ("M-S-<Return>", spawn "urxvt")
  , ("M-b", sendMessage ToggleStruts)
  , ("M-k", spawn "xkill")
  , ("M-p", spawn "dmenu_run")
  , ("M-<Space>", scratchpadSpawnActionCustom "urxvt -name scratchpad")
  , ("M-g", windowPromptGoto myXPConfig { autoComplete = Just 250000 })
  , ("M-S-g", windowPromptBring myXPConfig { autoComplete = Just 250000 })
  ]

myLogHook xmproc = dynamicLogWithPP xmobarPP
  { ppOutput = hPutStrLn xmproc
  , ppTitle  = xmobarColor "#ca8f2d" "" . shorten 72
  , ppLayout = const ""
  , ppHidden = \ws -> if ws == "NSP" then "" else ws
  , ppCurrent = xmobarColor "yellow" ""
  }

myStartupHook = do
  setWMName "LG3D"
  takeTopFocus
  spawn "rwall"

myXPConfig = amberXPConfig { font = "xft:DejaVu Serif:size=14"
                           , promptBorderWidth = 0
                           , searchPredicate = isInfixOf
                           }
