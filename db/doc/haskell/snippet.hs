$ghci -i "$HOME/.xmonad/lib" ~/.xmonad/xmonad.hs

$ghc -e 'System.Directory.getAppUserDataDirectory "xmonad"'

$ghc --info | egrep 'split|Host'
,("Host platform","i386-unknown-freebsd")
,("Object splitting supported","YES")

$cabal install xmonad-contrib --with-ghc=/home/sw2wolf/ghc/bin/ghc --enable-split-objs
$cabal install mighttpd2 --ghc-options=-fllvm
$cabal install hashable --constraint "unix==2.6.0.1" --constraint "bytestring==0.10.0.2" --dry-run
$cabal install --enable-library-profiling

---------
foreign import ccall "sin" c_sin :: CDouble -> CDouble
---------
flip runContT return $ callCC $
                  \exit -> forever $ do x < - getLine; when (x == "end") (exit ())
---------
[minBound, maxBound] :: String
"\NUL\1114111"

---------
let fibs = 0:1:zipWith (+) fibs (tail fibs) in fibs !! 70000
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

---------
There are 3 major parallel and concurrent programming models in Haskell.
.implicit parallelism via par
.explicit concurrency and parallelism via forkIO / MVars and software transactional memory
.data parallelism via the DPH libraries
---------
{-# LANGUAGE DeriveDataTypeable #-}

import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Timer

...

-- wrapper for the Timer id, so it can be stored as custom mutable state
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
  initialValue = TID 0

...

-- put this in your startupHook
-- start the initial timer, store its id
clockStartupHook = startTimer 1 >>= XS.put . TID

-- put this in your handleEventHook
clockEventHook e = do               -- e is the event we've hooked
  (TID t) <- XS.get                 -- get the recent Timer id
  handleTimer t e $ do              -- run the following if e matches the id
    startTimer 1 >>= XS.put . TID   -- restart the timer, store the new id
    ask >>= logHook.config          -- get the loghook and run it
    return Nothing                  -- return required type
  return $ All True                 -- return required type
------
Functor < Applicative < Monad.
where, "x < y" means x is a superclass of y.
All monads are conceptually applicative as well.
So if you can define a function to work on applicatives instead of monads, it's more flexible in that it should still work on all monads but also works on non-monadic applicatives.
------
