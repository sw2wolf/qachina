
---------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Monoid

newtype Diff a = Diff (Endo a) deriving Monoid

diff :: Monoid a => a -> Diff a
diff k = Diff (Endo (k <>))

getDiff :: Monoid a => Diff a -> a
getDiff (Diff (Endo k)) = k mempty

main :: IO ()
main = print $ getDiff $ diff ("Foo" :: String) <> diff "Bar" <> diff "Baz"

---------
flip runContT return $ callCC $
                  \exit -> forever $ do x < - getLine; when (x == "end") (exit ())
---------
[minBound, maxBound] :: String
"\NUL\1114111"
---------
ghci -i"$HOME/.xmonad/lib" ~/.xmonad/xmonad.hs
---------
let fibs = 0:1:zipWith (+) fibs (tail fibs) in fibs !! 70000
---------
import XMonad.Hooks.DynamicLog (xmobar)

main = do
    nScreens <- countScreens    -- just in case you are on a laptop like me count the screens so that you can go
    xmonad =<< xmobar myBaseConfig
      { modMask = myModMask
      , workspaces = withScreens nScreens myWorkspaces
      , layoutHook = myLayoutHook nScreens
      , manageHook = myManageHook
      , borderWidth = myBorderWidth
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , keys = myKeys
      , mouseBindings = myMouseBindings
      , logHook = myLogHook
      }
    where
        myLogHook = dynamicLogXinerama

myBaseConfig = gnomeConfig
-- Config { font = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
--        , bgColor = "black"
--        , fgColor = "grey"
--        , position = TopW L 85
--        , commands = [ Run Network "wlan0" ["-L","0","-H","32","--normal","green","--high","red"] 10
--                     , Run Cpu ["-L","15","-H","50","--normal","green","--high","red"] 10
--                     , Run Memory ["-t","Mem: %"] 10
--                     , Run Swap [] 10
--                     , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
--                     , Run StdinReader
--                     ]
--        , sepChar = "%"
--        , alignSep = "}{"
--        , template = "%StdinReader% }{ %cpu% | %memory% * %swap% | %wlan0% | %date%"
--        }

-- Config { font = "xft:Bitstream Vera Sans Mono:size=8:antialias=true"
--        , bgColor = "black"
--        , fgColor = "grey"
--        , position = Top
--        , lowerOnStart = True
--        , commands = [ Run Network "eth0" ["-L","0","-H","32","--normal","green","--high","red"] 10
--                     , Run Network "eth1" ["-L","0","-H","32","--normal","green","--high","red"] 10
--                     , Run Com "uname" ["-s","-r"] "" 36000
--                     , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
--                     ]
--        , sepChar = "%"
--        , alignSep = "}{"
--        , template = "| %eth0% - %eth1% }{ <fc=#ee9a00>%date%</fc>| %uname%"
--        }

---------
import System
import System.IO
import System.Process (runProcess, waitForProcess)
import System.Directory (removeFile)

import Control.Monad (replicateM_)
import Control.Parallel (pseq)

import qualified Data.ByteString as B

waitForProcess =<< runProcess "sdcv" ["-n", "apple"] (Just dir) Nothing Nothing Nothing (Just h)
hSetBuffering stdout NoBuffering
my_system str = do
  (_,_,_,p) <- createProcess c
  waitForProcess p
  where c = CreateProcess { cmdspec = ShellCommand str
                           ,cwd = Nothing
                           ,env = Nothing
                           ,std_in = Inherit
                           ,std_out = Inherit
                           ,std_err = Inherit
                           ,close_fds = True }

run :: FilePath -> IO ()
run exe = do
  let tempFile = "mytempfile.txt"
  h <- openFile tempFile WriteMode
  exitCode <- waitForProcess =<< runProcess exe [] Nothing Nothing Nothing (Just h) (Just h)
  hClose h >> (if exitCode /= ExitSuccess then return () else B.readFile tempFile >>= B.putStr) >> removeFile tempFile

main = replicateM_ 100 (putStrLn "Next:" >> run "a.exe")

---------
confirm :: String -> X () -> X ()
confirm m f = do
  result <- dmenu [m]
  when (init result == m) f

confirm "Restart" $ spawn "xmonad --recompile && xmonad --restart"
--or
confirm "Exit" $ io (exitWith ExitSuccess)

message s = menuArgs "dmenu" ["-p","result:", "-l","20","-nb","#000000", "-nf", "#FFFFFF"] [s] >> return ()

---------

import XMonad
import XMonad.Hooks.FadeInactive

myManageHook = manageDocks <+> composeAll
    [ title =? "uxterm"             --> (ask >>= \w -> liftX (setOpacity w 0.75) >> idHook)
    , className =? "Thunar"         --> (ask >>= \w -> liftX (setOpacity w 0.75) >> idHook)
    , className =? "Conky"          --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    ]

---------

(do x <- id; y <- reverse; z <- map toUpper; return (x,y,z)) "hello"
("hello","olleh","HELLO")

sequence [id, (+2), (*2), (^2), (2^)] 5
<lambdabot>   [5,7,10,25,32]

---------

import Data.Array.Vector
main = print (sumU (enumFromToU 1 (200000000 :: Int)))

---------

$ghc -e 'System.Directory.getAppUserDataDirectory "xmonad"'

$ghc --info | egrep 'split|Host'
,("Host platform","i386-unknown-freebsd")
,("Object splitting supported","YES")

$cabal install xmonad-contrib --with-ghc=/home/sw2wolf/ghc/bin/ghc --enable-split-objs
$cabal install mighttpd2 --ghc-options=-fllvm
$cabal install hashable --constraint "unix==2.6.0.1" --constraint "bytestring==0.10.0.2" --dry-run

---------

@unmtl StateT [e] Maybe a
<lambdabot> [e] -> Maybe (a, [e])

@undo [y | x <- xs, y <- f x]
<lambdabot> concatMap (\ x -> concatMap (\ y -> [y]) f x) xs

---------

-- > (-->) :: Monoid m => Query Bool -> Query m -> Query m -- a simpler type
(-->) :: (Monad m, Monoid a) => m Bool -> m a -> m a
p --> f = p >>= \b -> if b then f else return mempty

---------

-- | Ignore SIGPIPE to avoid termination when a pipe is full, and SIGCHLD to
-- avoid zombie processes, and clean up any extant zombie processes.
installSignalHandlers :: MonadIO m => m ()
installSignalHandlers = io $ do
    installHandler openEndedPipe Ignore Nothing
    installHandler sigCHLD Ignore Nothing
    (try :: IO a -> IO (Either SomeException a))
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    return ()

---------

-- | A replacement for 'forkProcess' which resets default signal handlers.
xfork :: MonadIO m => IO () -> m ProcessID
xfork x = liftIO . forkProcess . finally nullStdin $ do
                uninstallSignalHandlers
                createSession
                x
 where
    nullStdin = do
        fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
        dupTo fd stdInput
        closeFd fd

---------

case () of
    _ | W.member w s && W.peek s /= Just w -> windows (W.focusWindow w)
      | Just new <- mnew, w == root && curr /= new
                                          -> windows (W.view new)
      | otherwise                         -> return ()

---------

-- | Launch an external application through the system shell and return a @Handle@ to its standard input.
spawnPipe :: MonadIO m => String -> m Handle
spawnPipe x = io $ do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    _ <- xfork $ do
          _ <- dupTo rd stdInput
          executeFile "/bin/sh" False ["-c", encodeString x] Nothing
    closeFd rd
    return h

---------

import Data.Vector.Unboxed as U
 
sumSqrV :: U.Vector Int -> Int
sumSqrV = U.sum . U.map (^2) . U.filter odd

---------

fib :: Int -> Int
fib n = fib' !! n where
    fib' :: [Int]
    fib' = 0 : 1 : zipWith (+) fib' (tail fib')

---------

import XMonad
import XMonad.Actions.Volume
import XMonad.Util.Dzen
import Data.Map    (fromList)
import Data.Monoid (mappend)

-- onCurr :: (ScreenId -> DzenConfig) -> DzenConfig
-- center 150 66 :: ScreenId -> DzenConfig

-- font :: String -> DzenConfig
-- addArgs :: [String] -> DzenConfig
-- (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c
-- centered :: (Int, [String]) -> XMonad.Core.X (Int, [String])
type DzenConfig = (Int, [String]) -> XMonad.Core.X (Int, [String])

alert = dzenConfig centered . show . round
centered =
        onCurr (center 150 66)
    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> addArgs ["-fg", "#80c0ff"]
    >=> addArgs ["-bg", "#000040"]

main = xmonad defaultConfig { keys =
    keys defaultConfig `mappend`
    \c -> fromList [
        ((0, xK_F6), lowerVolume 4 >>= alert),
        ((0, xK_F7), raiseVolume 4 >>= alert)
    ]
}

-- message s = dzenConfig centered s
-- centered = timeout 5
--     >=> onCurr (center 150 66)
--     >=> font "-*-helvetica-*-r-*-*-12-*-*-*-*-*-*-*"
--     >=> addArgs ["-fg", "#80c0ff"]
--     >=> addArgs ["-bg", "#000040"]
--     >=> addArgs ["-l", "20""]

---------

import Data.List

mean xs = tot / len
  where
    (tot, len) = foldl' fun (0, 0) xs
    fun (!tot, !len) x = (tot+x, len+1)

