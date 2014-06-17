
-----
import Data.Bits               ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Int                (Int8, Int16, Int32, Int64)
import Data.IORef              (atomicModifyIORef, newIORef)
import Data.Ratio              ((%), numerator)

-----
{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}
{-# LANGUAGE NoMonomorphismRestriction     #-}

-----
{-# LANGUAGE BangPatterns #-}
import System.Random.MWC (createSystemRandom, uniformR, GenIO)
import Data.Int (Int64)
import Data.Bits (setBit)
import Control.Monad (replicateM, replicateM_)

makeMask :: Int -> GenIO -> IO [Int64]
makeMask p g = replicateM 50 $ singleMask p g

singleMask :: Int -> GenIO -> IO Int64
singleMask p g = go 0 [1..39]
  where
     go :: Int64# -> [Int] -> IO Int64
     go !a []     = return a
     go !a (x:xs) = do c <- uniformR (1,10000) g
                       go (if c <= p then setBit a x else a) xs

app :: GenIO -> IO ()
app g = replicateM_ 10000 $ (makeMask 10 g >>= print)

main :: IO ()
main = createSystemRandom >>= app

------
> let (+) :: Int -> Int -> Int; (+) = (-); infixr + in 5 + 3 + 2
4
------
parMap :: (b -> a) -> [b] -> IO [a]
parMap f xs = do
  outs <- replicateM t newEmptyMVar
  ins  <- replicateM t newEmptyMVar
  sequence_ [forkIO (worker i o) | (i,o) <- zip ins outs]
  forkIO $ sequence_ [putMVar i (f x) | (i,x) <- zip (cycle ins) xs]
  sequence' [takeMVar o | (o,_) <- zip (cycle outs) xs]

worker :: MVar a -> MVar a -> IO b
worker imv omv = forever $ do 
  ex <- takeMVar imv 
  ex `seq` putMVar omv ex

------
--git log -p ..origin/master
--git show 'HEAD@{1}..HEAD'

------
import Data.Time

time op = 
    getCurrentTime >>= \ t0 -> 
    op >> 
    getCurrentTime >>= \ tf -> 
    return $! (diffUTCTime tf t0)

------
:set -isrc
:l src/Misc.hs
:set prompt ">> "
:set -Wall
:set -fno-warn-unused-binds
:set -fno-warn-unused-do-bind
:set -fno-warn-unused-imports
:set -fno-warn-type-defaults
:set -XNoImplicitPrelude
:set -XScopedTypeVariables
:set -XOverloadedStrings

------
:set -fbreak-on-exception
:trace functionName
:hist

------
data Free f a = Pure a | Free (f (Free a))
-- A free monad is the least amount of structure that you need to add to a functor to make it a monad. There are more rigorous mathematical definitions, but that's the gist.

-- "Free f a" is an ADT that has a Monad instance. However, it requires that 'f' is a Functor. There's an instance Functor f => Monad (Free f). I'm not sure it's a helpful gist unless you already know what it  means. The point of gists is that they're sufficient for rudimentary understanding. So it is roughly said that Free makes any Functor a Monad: "Got a functor? Here's a monad... for free!". So you can write e.g. "data Four a = Four a a a a" and a Functor instance for it and then "Free Four a" is a Quadtree and comes with a Monad instance. So it is really just a cute way of wording what is going on. After all, you could also say... have a type 'a'? here is a Monad!

------
newtype ByteString0 = BS0 ByteString

readFile0 :: FilePath -> IO ByteString0
readFile0 x = do
    src <- BS.readFile x
    return $ BS0 $ src `BS.snoc` '\0'

-- We define a newtype wrapper around ByteString so we gain some type safety. We also define a readFile0 that reads a file as a ByteString0, by explicitly calling snoc with \0. We can now define our own break0 function (this is the only big chunk of Haskell in this article):
break0 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break0 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! end `minusPtr` start

        go s | c == '\0' || f c = s
             | otherwise = go $ inc s
            where c = chr s

chr :: Ptr Word8 -> Char
chr x = Internal.w2c $ Internal.inlinePerformIO $ peek x

inc :: Ptr Word8 -> Ptr Word8
inc x = x `plusPtr` 1

------
fact n = runST (do
    r < - newSTRef 1
    for (1,n) (\x -> do
       val < - readSTRef r
       writeSTRef r (val * x))
    readSTRef r)

import Control.Monad
import Control.Monad.Reader

-- count :: Int -> IO Int
-- count n = do {r <- newIORef 0 ; 
--               loop r 1 }
--         where 
--           loop :: IORef Int -> Int -> IO Int
--           loop r i | i>n       = readIORef r
--                    | otherwise = do { v <- readIORef r ; 
--                                       writeIORef r (v+i) ;
--                                       loop r (i+1) }
hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2

main = print . runReader convo $ "adit"

------
import Control.Monad
import Random
import Data.IORef
import Text.Printf
 
randN :: Integer -> IO Bool
randN n = randomRIO (1,n) >>= return . (== 1)
 
unbiased :: Integer -> IO Bool
unbiased n = do
  a <- randN n
  b <- randN n
  if a /= b then return a else unbiased n
 
main :: IO ()
main = forM_ [3..6] $ \n -> do
  cb <- newIORef 0
  cu <- newIORef 0
  replicateM_ trials $ do
    b <- randN n
    u <- unbiased n
    when b $ modifyIORef cb (+ 1)
    when u $ modifyIORef cu (+ 1)
  tb <- readIORef cb
  tu <- readIORef cu
  printf "%d: %5.2f%%  %5.2f%%\n" n
    (100 * fromIntegral tb / fromIntegral trials :: Double)
    (100 * fromIntegral tu / fromIntegral trials :: Double)
  where trials = 50000

-- Output: 
-- 3: 33.72%  50.08%
-- 4: 25.26%  50.15%
-- 5: 19.99%  50.07%
-- 6: 16.67%  50.10%

------
--Endo is the monoid wrapper for (a -> a) with mappend = (.), mempty = id

Data.Monoid Endo :: (a -> a) -> Endo a
Data.Monoid appEndo :: Endo a -> a -> a

(appEndo $ (Endo (+1)) <> (Endo (+2))) 1
4

------
fac n = snd (until ((>n) . fst) (\(i,m) -> (i+1, i*m)) (1,1))

@guard (3 > 5) >> listToMaybe (drop 3 [1..9])
Nothing

------
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
      ...
      , ((modm              , xK_s),      getSelection  >>= sdcv)
      , ((modm .|. shiftMask, xK_s),      getDmenuInput >>= sdcv)
      ...
    ]

------
@where rts-xc
<lambdabot> ghc -prof -fprof-auto -rtsopts -osuf .p_o foo.hs && ./foo +RTS -xc
	# print stack traces on unhandled exceptions

@ty (?f >=> ?g)
(Monad m, ?f::a -> m b, ?g::b -> m c) => a -> m c

ghc -O2 -main-is StudentTest StudentTest.lhs -prof \
    -package-db=.cabal-sandbox/x86_64-osx-ghc-7.6.2-packages.conf.d

runghc Setup.lhs configure --enable-library-profiling \
--package-db=/HasBayes/.cabal-sandbox/x86_64-osx-ghc-7.6.2-packages.conf.d \
--libdir=/HasBayes/.cabal-sandbox/lib

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
data X = forall a. X a (a -> a) (a -> String)

-- the constructor (the one who constructs an X) chooses 'a'
-- the destructor doesn't know anything about 'a'

-- if you have an 'X x f gimme', then all the destructor knows is that f and gimme can be applied to x

------
import qualified Data.Vector as V
main = print (V.foldl' (+) 0 (V.enumFromTo 1 1000000000) :: Int)

import Data.Array.Vector
main = print (sumU (enumFromToU 1 (200000000 :: Int)))

import Data.Vector.Unboxed as U 
sumSqrV :: U.Vector Int -> Int
sumSqrV = U.sum . U.map (^2) . U.filter odd

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

-- The full output of log.txt is available here. It contains the GHC Core (which looks a bit like Haskell), then the C-- (which looks a bit like C) and finally the assembly code (which looks exactly like assembly).
ghc -c -O2 InnerLoop.hs -ddump-simpl -ddump-cmm -ddump-asm > log.txt

$cabal install xmonad-contrib --with-ghc=/home/sw2wolf/ghc/bin/ghc --enable-split-objs

$cabal install mighttpd2 --ghc-options=-fllvm
--Mighttpd2 supports HTTPS (HTTP over SSL/TLS) experimentally. To use it, type:
$cabal install --flags="tls" mighttpd2

$cabal install hashable --constraint "unix==2.6.0.1" --constraint "bytestring==0.10.0.2"
--dry-run
$cabal install --enable-library-profiling
$cabal install aeson --constraint="hashable==1.2.0.7" -v

--to build a cabal package packagename: 
$ cabal install --ghc-options=-package const-math-ghc-plugin -fplugin ConstMath.Plugin packagename

$ cabal install network --configure-option --host=i386-unknown-mingw32

$cabal unpack parconc-examples
$cabal install --only-dependencies
$cabal configure
$cabal build

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

-- (-->) :: Monoid m => Query Bool -> Query m -> Query m -- a simpler type
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

------
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
