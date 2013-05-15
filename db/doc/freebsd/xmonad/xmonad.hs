import qualified Data.Map as M
import System.IO
--import Graphics.X11.Xlib
--import Data.Char (isSpace)

import XMonad
import qualified XMonad.StackSet as W 

import XMonad.Actions.GridSelect
--import XMonad.Actions.WindowMenu
import XMonad.Actions.WindowGo

import XMonad.Hooks.ManageDocks
--import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
--import XMonad.Hooks.UrgencyHook
-- Java swing fix
import XMonad.Hooks.SetWMName (setWMName)

import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace  (onWorkspace)
import XMonad.Layout.LayoutHints   (layoutHintsWithPlacement)
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.Column
import XMonad.Layout.ShowWName

import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.Scratchpad

--myStatusBar = "dzen2 -x '0' -y '0' -h '24' -w '1280' -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn '-*-simsun-medium-r-normal-*-12-*-*-*-*-*-iso10646-1'"
--myBtmStatusBar = "conky -c /home/serrghi/.conky_bottom_dzen | dzen2 -x '0' -w '1280' -h '24' -ta 'c' -bg '#161616' -fg '#FFFFFF' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*' -y '776'"
--xmobar   = "~/.cabal/bin/xmobar"
--xmobarrc = "~/.xmonad/xmobarrc"

main = do
    --xmobar <- spawnPipe $ xmobar ++ " " ++ xmobarrc
    --dzenTopBar <- spawnPipe myStatusBar
    --dzenBtmBar <- spawnPipe myBtmStatusBar
    xmonad {- $ withUrgencyHook FocusHook -} $ {- ewmh -} defaultConfig {
        --handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
       borderWidth		= 1
      , focusedBorderColor 	= "#ff6666"
      , normalBorderColor 	= "#2222aa"
      , manageHook = manageHook defaultConfig <+> myManageHook
      , workspaces = ["1-work","2-game","3-test"] -- ++ map show [3..9]
      , terminal = "xterm"
      , modMask  = mod4Mask
      , focusFollowsMouse  = False
      --, clickToFocus = True
      , startupHook = myStartupHook
        --, logHook = myLogHook dzenTopBar
        --, logHook = myLogHook xmobar
      , logHook = myLogHook
      , layoutHook = showWName myLayout
   } `additionalKeys` myKeys
     `removeMouseBindings` [(mod4Mask,button1), (mod4Mask,button2), (mod4Mask,button3)]

myStartupHook :: X ()
myStartupHook = do 
	return ()
    --spawnOnce "fcitx"
    --spawnOnce "lxpanel"

myLogHook :: X ()
myLogHook = return ()

--myLogHook :: Handle -> X ()
-- myLogHook h = dynamicLogWithPP $ defaultPP
--       {   ppCurrent	= dzenColor "black" "green" . pad
-- 	, ppVisible	= dzenColor "black" "lightgreen" . pad
-- 	, ppHidden	= dzenColor "#cccccc" "" . pad
-- 	, ppHiddenNoWindows = dzenColor "#444444"  "" . pad
-- 	, ppUrgent	= dzenColor "" "red"
-- 	, ppWsSep    = " "
-- 	, ppSep      = " | "
-- 	, ppTitle    = (" " ++) . dzenColor "green" "" . dzenEscape
-- 	, ppOutput   = hPutStrLn h 
--       }

-- myLogHook xmproc = dynamicLogWithPP xmobarPP
--   { ppOutput = hPutStrLn xmproc
--   , ppTitle  = xmobarColor "#ca8f2d" "" . shorten 72
--   , ppLayout = const ""
--   , ppHidden = \ws -> if ws == "NSP" then "" else ws
--   , ppCurrent = xmobarColor "yellow" ""
--   }

myLayout = windowNavigation $ avoidStruts $ noBorders --smartBorders
           $ onWorkspace "1-work" simplestFloat
           $ onWorkspace "2-game" simplestFloat
           $ onWorkspace "3-test"  (Column 0.65)
           $ simplestFloat  --stdLayouts
     -- where
    --      stdLayouts = Mirror tall ||| tall ||| full      

    --      tall 	= Tall nmaster delta ratio
    --      full    = hinted (noBorders Full)

    -- -- like hintedTile but for any layout
    --      hinted l        = layoutHintsWithPlacement (0,0) l
    --      nmaster         = 1
    --      delta           = 3/100
    --      ratio           = toRational (2 / (1 + sqrt 5 :: Double)) -- golden ratio

myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [[className =? c --> doIgnore | c <- myIgnores]
    ,[className =? c --> doFloat | c <- myCFloats]
    ,[isFullscreen --> doFullFloat]
    ]) <+> manageTypes <+> manageDocks <+> scratchpadManageHook (W.RationalRect 0.50 0.375 0.50 0.60)
  where
    myIgnores = ["trayer", "desktop", "desktop_window"]
    myCFloats = ["GQview", "MPlayer", "Vncviewer","Xmessage"]
    role = stringProperty "WM_WINDOW_ROLE"
    name = stringProperty "WM_NAME"

manageTypes :: ManageHook
manageTypes = checkType --> doCenterFloat

checkType :: Query Bool
checkType = ask >>= \w -> liftX $ do
    m <- getAtom "_NET_WM_WINDOW_TYPE_MENU"
    d <- getAtom "_NET_WM_WINDOW_TYPE_DIALOG"
    u <- getAtom "_NET_WM_WINDOW_TYPE_UTILITY"
    mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w

    case mbr of
        Just [r] -> return $ elem (fromIntegral r) [m,d,u]
        _        -> return False

-- Theme {{{
-- Color names are easier to remember:
-- colorOrange          = "#ff7701"
-- colorDarkGray        = "#171717"
-- colorPink            = "#e3008d"
-- colorGreen           = "#00aa4a"
-- colorBlue            = "#008dd5"
-- colorYellow          = "#fee100"
-- colorWhite           = "#cfbfad"
 
-- colorNormalBorder    = "#1c2636"
-- colorFocusedBorder   = "#ebac54"
-- barFont  = "terminus"
-- barXFont = "inconsolata:size=14"
-- xftFont = "xft: inconsolata-14"
--}}}

-- Prompt Config {{{
-- myXPConfig :: XPConfig
-- myXPConfig = defaultXPConfig {
--           font                  = "xft:WenQuanYi Zen Hei:pixelsize=16"
--         , bgColor               = colorDarkGray
--         , fgColor               = colorGreen
--         , bgHLight              = colorGreen
--         , fgHLight              = colorDarkGray
--         , promptBorderWidth     = 0
--         , height                = 14
--         , historyFilter         = deleteConsecutive
-- }

-- Run"" or Raise Menu
-- largeXPConfig :: XPConfig
-- largeXPConfig = myXPConfig
--                 { font = xftFont
--                 , height = 20
--                 }
-- }}}

emacs = "emacs -geometry 176x34+0+368"
xterm="xterm -geometry 159x25+0+438"
eweiqi="wine \"c:/Program Files/eweiqi/LiveBaduk.exe\""
winxp="VBoxManage startvm winxp"

myKeys = let modm = mod4Mask in
    [ ((modm, xK_w), runOrRaise "opera" (className =? "Opera"))
    , ((modm, xK_e), runOrRaise emacs (className =? "Emacs"))
    , ((modm, xK_p), spawn "~/bin/dmenu.sh")
    --, ((modm, xK_p), spawnSelected defaultGSConfig [
    --          xterm, "gmrun", "opera", emacs, eweiqi, winxp])
    --, ((modm .|. shiftMask, xK_p),runOrRaisePrompt largeXPConfig)
    , ((modm, xK_g), goToSelected defaultGSConfig)
    , ((modm, xK_F11), spawn "sudo /sbin/shutdown -r now")
    , ((modm, xK_F12), spawn "sudo /sbin/shutdown -p now")
    --, ((modm .|. shiftMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((modm, xK_Print), spawn "scrot '/tmp/%Y-%m-%d_%H:%M:%S_$wx$h_scrot.png' -e 'mv $f ~'")
    , ((modm, xK_k), kill)
    , ((modm, xK_space), scratchpadSpawnActionCustom "xterm -name scratchpad") -- -e cl

    -- Window Navigation
    , ((modm, xK_Right), sendMessage $ Go R)
    , ((modm, xK_Left ), sendMessage $ Go L)
    , ((modm, xK_Up   ), sendMessage $ Go U)
    , ((modm, xK_Down ), sendMessage $ Go D)

    -- swap...
    , ((modm .|. controlMask, xK_Right), sendMessage $ Swap R)
    , ((modm .|. controlMask, xK_Left ), sendMessage $ Swap L)
    , ((modm .|. controlMask, xK_Up   ), sendMessage $ Swap U)
    , ((modm .|. controlMask, xK_Down ), sendMessage $ Swap D)
    --, ((modm .|. controlMask, xK_Up), windows W.swapUp)
    --, ((modm .|. controlMask, xK_Down), windows W.swapDown)
      
    -- sound control
    -- , ((modm .|. shiftMask, xK_Up), spawn "aumix -v+6") -- volume++ 
    -- , ((modm .|. shiftMask, xK_Down ), spawn "aumix -v-6") -- volume-- 
    -- , ((modm .|. shiftMask, xK_Left ), spawn "amixer set Master toggle") -- mute
    , ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile && xmonad --restart")
    ]
