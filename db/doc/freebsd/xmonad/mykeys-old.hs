-- myKeys = let modm = mod4Mask in
--     [ ((modm, xK_w), raiseMaybe (spawn "opera") (className =? "Opera"))
--     , ((modm, xK_e), raiseMaybe (spawn emacs) (className =? "Emacs"))
--     , ((modm, xK_p), spawn "~/bin/dmenu.sh")
--     --, ((modm, xK_p), spawnSelected defaultGSConfig [
--     --          xterm, "gmrun", "opera", emacs, eweiqi, winxp])
--     --, ((modm .|. shiftMask, xK_p),runOrRaisePrompt largeXPConfig)
--     , ((modm, xK_g), goToSelected defaultGSConfig)

--    --, ((modm, xK_c), inputPrompt myXPConfig "Word" >>= flip whenJust (\word-> spawn $ "sdcv -n " ++ word ++ "|zenity --text-info --width 530 --height 300"))
--    --, ((modm, xK_c), inputPrompt myXPConfig "Word" ?+ (\word-> spawn $ "~/bin/sdcv.sh " ++ word))
--    , ((modm, xK_c), spawn "~/bin/sdcv.sh")
--    , ((modm, xK_x), spawn "~/bin/clisp.sh")

--    , ((modm .|. controlMask, xK_n), do
--             spawn ("date>>" ++ "~/TODO")
--             appendFilePrompt myXPConfig "/home/sw2wolf/TODO"
--      )

--    --, ((modm .|. controlMask, xK_x), shellPrompt myXPConfig)
--    , ((modm, xK_F11), spawn "sudo /sbin/shutdown -r now")
--    , ((modm, xK_F12), spawn "sudo /sbin/shutdown -p now")

--    --, ((modm .|. shiftMask, xK_Print), spawn "sleep 0.2; scrot -s")
--    --, ((modm, xK_Print), spawn "scrot '/tmp/%Y-%m-%d_%H:%M:%S_$wx$h_scrot.png' -e 'mv $f ~'")
--    , ((modm, xK_k), kill)

--    --, ((modm, xK_space), namedScratchpadAction scratchpads "xterm")
--    , ((modm, xK_space), raiseMaybe (spawn xterm) (className =? "XTerm"))

--     -- Window Navigation
--    , ((modm, xK_Right), sendMessage $ Go R)
--    , ((modm, xK_Left ), sendMessage $ Go L)
--    , ((modm, xK_Up   ), sendMessage $ Go U)
--    , ((modm, xK_Down ), sendMessage $ Go D)

--     -- swap...
--    , ((modm .|. controlMask, xK_Left ), sendMessage $ Swap L)
--    , ((modm .|. controlMask, xK_Right), sendMessage $ Swap R)
--    , ((modm .|. controlMask, xK_Up   ), sendMessage $ Swap U)
--    , ((modm .|. controlMask, xK_Down ), sendMessage $ Swap D)
--     --, ((modm .|. controlMask, xK_Up), windows W.swapUp)
--     --, ((modm .|. controlMask, xK_Down), windows W.swapDown)
      
--     -- sound control
--     -- , ((modm .|. shiftMask, xK_Up), spawn "aumix -v+6") -- volume++ 
--     -- , ((modm .|. shiftMask, xK_Down ), spawn "aumix -v-6") -- volume-- 
--     -- , ((modm .|. shiftMask, xK_Left ), spawn "amixer set Master toggle") -- mute
--    , ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile && xmonad --restart")
--     ]