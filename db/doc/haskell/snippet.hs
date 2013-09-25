
------
:t (?f >=> ?g)
(Monad m, ?f::a -> m b, ?g::b -> m c) => a -> m c
------
Data.Monoid Endo :: (a -> a) -> Endo a
Data.Monoid appEndo :: Endo a -> a -> a

(appEndo $ (Endo (+1)) <> (Endo (+2))) 1
4
------
ghci> let double :: Int -> Int; double x = x + x

You can also use :{ and :} to do a muli-line definition:
ghci> :{
Prelude| let double :: Int -> Int
Prelude|     double x = x + x
Prelude| :}
ghci> 
--I recommend doing most of your work in a text editor, and then load the file into ghci (with :load, or providing it as an argument on the command line) and playing with it. I don't find ghci terribly pleasant to work with when actually writing code -- it's much better at messing around with code that's already written. Whenever you modify the text file, :reload (or just :r) in ghci.
------
universal means caller/destructor chooses, existential
		   means callee/constructor chooses  [13:53]
universal:  f :: forall a. a -> (a -> a) -> a  [13:54]
caller chooses 'a'

data X = forall a. X a (a -> a) (a -> String)
the constructor (the one who constructs an X) chooses 'a'
the destructor doesn't know anything about 'a'
if you have an 'X x f gimme', then all the destructor knows is that f and gimme can be applied to x
------
import qualified Data.Vector as V
main = print (V.foldl' (+) 0 (V.enumFromTo 1 1000000000) :: Int)

import Data.Array.Vector
main = print (sumU (enumFromToU 1 (200000000 :: Int)))
------
{-# LANGUAGE ScopedTypeVariables #-}

retryOnTimeout :: IO a -> IO a
retryOnTimeout action = catch action $ \ (_ :: HttpException) -> do
  putStrLn "Timed out. Trying again."
  threadDelay 5000000
  action
------
.Data.Map.Map is a balanced binary tree internally, so its time complexity for lookups is O(log n). I believe it's a "persistent" data structure, meaning it's implemented such that mutative operations yield a new copy with only the relevant parts of the structure updated.
.Data.HashMap.Map is a Data.IntMap.IntMap internally, which in turn is implemented as Patricia tree; its time complexity for lookups is O(min(n, W)) where W is the number of bits in an integer. It is also "persistent."
.Data.HashTable.HashTable is an actual hash table, with time complexity O(1) for lookups. However, it is a mutable data structure -- operations are done in-place -- so you're stuck in the IO monad if you want to use it.
------
[0..] >>= \n -> n^2 <$ guard (even n)
[0,4,16 ...]

[minBound, maxBound] :: String
"\NUL\1114111"

let fibs = 0:1:zipWith (+) fibs (tail fibs) in fibs !! 70000

(do x <- id; y <- reverse; z <- map toUpper; return (x,y,z)) "hello"
("hello","olleh","HELLO")

sequence [id, (+2), (*2), (^2), (2^)] 5
[5,7,10,25,32]
------
$ghci -i "$HOME/.xmonad/lib" ~/.xmonad/xmonad.hs

$ghc -e 'System.Directory.getAppUserDataDirectory "xmonad"'

$ghc --info | egrep 'split|Host'
,("Host platform","i386-unknown-freebsd")
,("Object splitting supported","YES")

$cabal install xmonad-contrib --with-ghc=/home/sw2wolf/ghc/bin/ghc --enable-split-objs
$cabal install mighttpd2 --ghc-options=-fllvm
$cabal install hashable --constraint "unix==2.6.0.1" --constraint "bytestring==0.10.0.2" --dry-run
$cabal install --enable-library-profiling
$cabal install aeson --constraint="hashable==1.2.0.7" -v

---------
foreign import ccall "sin" c_sin :: CDouble -> CDouble

---------
flip runContT return $ callCC $
                  \exit -> forever $ do x < - getLine; when (x == "end") (exit ())

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
fix can make recursive lambdas
(a -> a) -> a
@src fix
<lambdabot> fix f = let x = f x in x
------
showFFloat (Just 2) 9.575 ""  => "9.58"
------
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import Data.Char
import System.IO
import System.Process
import System.Exit
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as C

main = do
   script <- getArgs >>= \(file:blather) -> readFile file
   ty     <- run "ghci" ("-v0" : "-cpp" : "-w" : args) script
   putStr ("evaluating Hello.haskellscript\n\n" ++ ty)
   
  where args = ["-XPostfixOperators"]

run :: FilePath -> [String] -> String -> IO String
run file args input = C.handle (\(e :: C.IOException) -> return (show e)) $ do

   (inp,out,err,pid) <- runInteractiveProcess file args Nothing Nothing
   hPutStr inp input >> hClose inp

   output <- hGetContents out
   errput <- hGetContents err

   outMVar <- newEmptyMVar
   errMVar <- newEmptyMVar

   forkIO (C.evaluate (length output) >> putMVar outMVar ())
   forkIO (C.evaluate (length errput) >> putMVar errMVar ())

   takeMVar outMVar
   takeMVar errMVar

   e <- C.catch
           (waitForProcess pid)
           (\(_ :: C.IOException) -> return ExitSuccess)

   return (output ++ errors ++ better errput)
   
errors = "\n\n**********************************"
better  = unlines . map linemanager . lines
  where linemanager l  = 
         case (take 13 l, drop 13 l) of
            ("<interactive>", xs) -> "YOUR SCRIPT DOESNT MAKE SENSE!\n" ++ tail xs
            _                     -> l
------
let inf ~(x:xs) = x : inf xs in length . take 5 $ inf [] => 5
inf ~(x:xs) = ...  says you only actually look at the LHS  if anyone ever asks for the value of "x" or "xs".  But nobody does. try to read it as:  inf ? = ? : inf ? note that if nobody asks the question marks what they are, it's basically like a repeat
