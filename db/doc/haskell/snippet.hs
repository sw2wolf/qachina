---------

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

@undo [y | x <- xs, y <- f x]
<lambdabot> concatMap (\ x -> concatMap (\ y -> [y]) f x) xs

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

$ghc --info | egrep 'split|Host'  [08:42]
<sw2wolf>  ,("Host platform","i386-unknown-freebsd")
<sw2wolf>  ,("Object splitting supported","YES")

$cabal install xmonad-contrib --with-ghc=/home/sw2wolf/ghc/bin/ghc --enable-split-objs

---------

import Data.List

mean xs = tot / len
  where
    (tot, len) = foldl' fun (0, 0) xs
    fun (!tot, !len) x = (tot+x, len+1)

