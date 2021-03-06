{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Money (
    winG, winQ, div618, stopLoss
   ,win_ssq, hit_ssq, his
   ,qachina, combLen, fibs, sieves
             )
where
import System.Random
import System.IO
--import System.Time
--import System.Directory
import System.Environment()
import System.Process
import System.Exit

import Text.Printf (printf)
--import Text.Regex

import Data.List
--import Data.IORef
--import Data.Time.Calendar
--import Data.Time.Calendar.WeekDate 

import qualified Control.Exception as Exception

import Control.Monad
import Control.Applicative

import Control.Arrow
import Control.Concurrent

import Foreign.Marshal.Alloc
import Foreign.Storable

#if __GLASGOW_HASKELL__ >= 610
catchAny :: forall a.
                  IO a -> (Exception.SomeException -> IO a) -> IO a
catchAny f h = Exception.catch f (\e -> h (e :: Exception.SomeException))
#else
catchAny = Exception.catch
#endif

-- 菲波纳契数列
--fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]

fibs :: forall a. Num a => Int -> [a]
fibs n = take n $ fibgen 1 1

fibgen :: forall a. Num a => a -> a -> [a]
fibgen n1 n2 = n1 : fibgen n2 (n1+n2) 

--求解素数的一个无限数列方法：
sieves :: forall a. Integral a => Int -> [a]
sieves n = take n $ sieve []

sieve :: forall a. Integral a => [a] -> [a]
sieve [] = sieve [2..]
sieve (x:xs) = x : sieve (filter (\y -> y `rem` x /= 0) xs)

--排列组合 same as Data.List.subsequences
-- combination :: [a] -> [[a]]
-- combination [] =  [[]]
-- combination (x:xs) = concat [[(x:ys), ys] | ys <- combination xs]

-- permutation :: Eq a => [a] -> [[a]]
-- permutation [] = [[]]
-- permutation xs = concatMap (\x -> map (x:) $ permutation (delete x xs)) xs

combLen :: Int -> Int -> Int
combLen x len
  | x > len = product [x-len+1..x] `div` product [1..len]
  | otherwise = 0

qachina :: IO ()
qachina = do
    -- putStrLn "Running qachina..."
    -- runCommand "mighty mighty.conf mighty.route" >>= waitForProcess
    return ()
    --pid <- runCommand "mighty mighty.conf mighty.route"
    --waitForProcess pid >>= exitWith

------------------------------------------------------------------------------

sxf :: Float
sxf = 0.0015  --手续费

yhs :: Float
yhs = 0.001   --印花费

ghf :: Float
ghf = 1.0     --过户费

--计算股票盈利
winG :: Float -> Float -> Float -> Float
winG qty pb ps = qty * ps * (1 - sxf - yhs) - 2 * ghf - qty * pb * (1 + sxf)

--算权证盈利
winQ :: Float -> Float -> Float -> Float
winQ qty pb ps = qty * ps * (1 - sxf) - 2 * ghf - qty * pb * (1 + sxf)

--止损价
stopLoss :: Float -> Float -> Float -> IO ()
stopLoss qty pb lossRate = do
    let t = qty * pb * (1 + sxf)
    printf "Stop Loss at: %.2f\n" ((pb - (t * lossRate) / qty) :: Float)
    printf "Lost Money: %.2f\n"  ((t * lossRate) :: Float)

div618 :: Double -> Double -> IO ()
div618 p1 p2 = do
    let price r = if p1 <= p2 then p1 + (p2 - p1) * r else p1 - (p1 - p2) * r
        ratio = [0,0.191,0.236,0.382,0.5,0.618,0.809,1] 
    if p1 <= p2
        then  mapM (\r -> printf "-------%.3f   %.2f-------\n" r $ price r) $ reverse ratio
        else  mapM (\r -> printf "-------%.3f   %.2f-------\n" r $ price r) $ ratio
        
    return ()

-- fG y m d = fromGregorian y m d

-- no_trade_days = [fG 2011 1 1, fG 2011 1 2, fG 2011 1 3, --元旦
--                  fG 2011 2 2, fG 2011 2 3, fG 2011 2 4, fG 2011 2 5, fG 2011 2 6, --春节
--                  fG 2011 2 7, fG 2011 2 8,
--                  fG 2011 4 3, fG 2011 4 4, fG 2011 4 5, --清明
--                  fG 2011 4 30, fG 2011 5 1, fG 2011 5 2, --劳动
--                  fG 2011 6 4, fG 2011 6 5, fG 2011 6 6,  --端午
--                  fG 2013 9 19, fG 2013 9 20, fG 2013 9 21,  --中秋
--                  fG 2011 10 1, fG 2011 10 2, fG 2011 10 3, fG 2011 10 4, fG 2011 10 5,  --国庆
--                  fG 2011 10 6, fG 2011 10 7
--                 ]

-- is_trade_day day =
--     if (elem wd [6,7]) || (elem day no_trade_days) then False else True
--     where (y,w,wd) = toWeekDate day

--dateWindow sDayStr n = mapM_ (\nday -> dayByNTDay  sDayStr nday) $ fibs n
{--
dayByNTDay sDayStr nDay = do {
    let {
        [y,m,d] = map (\x -> read x::Int) $ splitRegex (mkRegex "-") sDayStr;
        sDay = fG (fromIntegral y) m d
    };
    print $ find_the_day sDay nDay ;
}
--}
-- find_the_day sDay 0 = sDay
-- find_the_day sDay nDay = 
--     let nextDay = addDays 1 sDay
--     in if (is_trade_day $ nextDay)
--     then find_the_day nextDay (nDay - 1)
--     else find_the_day nextDay nDay

------------------------------------------------------------------------
-- rollDice :: Int -> IO Int
-- rollDice n = do 
--     tmp <- doesFileExist "/dev/urandom" 
--     myGen <- if tmp
--         then mkStdGen <$> betterSeed
--         else (mkStdGen . fromInteger) <$> picoSec
    
--     return $ (take 1 $ randomRs (1,n) myGen) !! 0

-- picoSec :: IO Integer
-- picoSec = ctPicosec <$> (getClockTime >>= toCalendarTime)

betterSeed :: IO Int
betterSeed = alloca $ \p -> do
    withBinaryFile "/dev/urandom" ReadMode $ \h ->
        hGetBuf h p $ sizeOf (undefined :: Int)
    peek p

------------------------------------------------------------------------
win_ssq :: Int -> String -> String -> IO ()
win_ssq count noRed noBlue = do
    --let noStr = (map (\x->show x) noRed) ++ ["-"] ++ (map (\x->show x) noBlue)
    --writeFile "noRedBlue.txt" $ concat $ intersperse " " noStr
    let noRedLst =  map (\x -> read x::Int) $ words noRed
        noBlueLst = map (\x -> read x::Int) $ words noBlue

    --gRed <- goodRed

    _ <- setStdGen <$> (mkStdGen <$> betterSeed)
    okBlue <- pickNums ([1..16] \\ noBlueLst) count []
    result <- pickSSQ count
              ([1..33] \\ [1,33])
              ([1..33] \\ noRedLst)
              okBlue []
    --forM_ result (\x -> print x)
    writeFile ssqNum $ ints2str result

pickSSQ :: Int -> [Int] -> [Int] -> [Int] -> [[Int]] -> IO [[Int]]
pickSSQ 0 _ _ _ acc = return acc
pickSSQ 1 xRed _ okBlue acc = do
    red <- sort <$> pickNums xRed 6 []
    let one = red ++ [okBlue!!0]
    print one
    return $ reverse $ one : acc
pickSSQ count xRed yesRed okBlue acc = do
    red <- sort <$> pickNums yesRed 6 []
    let one = red ++ [okBlue!!(count-1)]
    print one
    pickSSQ (count-1) xRed yesRed okBlue $ one:acc

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

pickNums :: [Int] -> Int -> [Int] -> IO [Int]
pickNums _ 0 acc = return acc
pickNums from count acc = do
  x <- pick from
  threadDelay 1000000 -- 1 second
  pickNums (from \\ [x]) (count-1) (x:acc)

ints2str :: [[Int]] -> String
ints2str ints = concat $ intersperse "\n" strLst
    where
        strLst = map (\x -> to_str x) ints
        to_str a = unwords $ map (\x -> show x) a 

-- goodRed:: IO [Int]
-- goodRed = do {
--     samp <- fmap (concat . str_ints_hit) $ readFile ssqHitNum;
--     return $ sort $ take 21 $ map (\(a,_) -> a) $ statis samp;
-- }

statis :: [Int] -> [(Int,Int)]
statis samp = map (\(a,b) -> (b,a)) $ times4n
    where
        times4n = (reverse . sort) $ map (length &&& head) $ (group . sort) samp

--skip the 期号
str_ints_hit :: String -> [[Int]]
str_ints_hit str = map (\line -> map (\x->read x::Int) line) $ map (init . tail . words) $ lines str

str_ints_pick :: String -> [[Int]]
str_ints_pick str = map (\line -> map (\x->read x::Int) line) $ map words $ lines str

hit_ssq :: String -> String -> IO ()
hit_ssq no hitNum = do
    let hitLst =  map (\x -> read x::Int) $ words hitNum
    let hitRed redNo = foldl (\acc x -> if(x `elem` (init hitLst)) then acc+1 else acc) 0 redNo
    dats <- readFile ssqHitNum
    if (any (\x -> no `elem` (words x)) (lines dats) == False)
        then catchAny (appendFile ssqHitNum $ unwords $ [no] ++ map (\n -> show n) hitLst ++ ["\n"]) (\e -> fail $ "Unable save hit numbers" ++ show e)
        else return ()

    --gRed <- goodRed
    --printf "good red hit %d\n" ((hitRed gRed) :: Int)

    nums <- fmap str_ints_pick $ readFile ssqNum
    forM_ nums (\n -> do
        let hitR = hitRed $ init n
            hitB = if (n!!6 == hitLst!!6) then 1 else 0 
        printf "%s\t%d:%d\t%s\n" (show n) (hitR::Int) (hitB::Int) (hit_desc hitR hitB))

hit_desc :: forall a a1.
                  (Eq a, Eq a1, Num a, Num a1) =>
                  a -> a1 -> [Char]
hit_desc red blue
    | red == 6 && blue == 1 = "First"
    | red == 6 && blue == 0 = "Second"
    | red == 5 && blue == 1 = "Third(3000)"
    | (red == 5 && blue == 0) || (red == 4 && blue == 1) = "Fourth(200)"
    | (red == 4 && blue == 0) || (red == 3 && blue == 1) = "Fifth(10)"
    | (blue == 1) = "Sixth(5)"
    | otherwise = "X" 

his :: IO ()
his = do
  pid <- runCommand $ "tail " ++ ssqHitNum
  waitForProcess pid >>= exitWith

ssqNum :: [Char]
ssqNum = "/media/D/qachina/db/doc/money/" ++ "ssqNum.txt"

ssqHitNum :: [Char]
ssqHitNum = "/media/D/qachina/db/doc/money/" ++ "ssqHitNum.txt"

------------------------------------------------------------------------
