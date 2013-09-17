{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Money (
    winG, winQ, div618, stopLoss
    ,qachina, fibs
    ,winSSQ, hitSSQ
    )
where
import System.Random
import System.IO
import System.Time
import System.Directory
import System.Environment
import System.Process
import System.Exit

import Text.Printf (printf)
--import Text.Regex

import Data.List
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate 

import qualified Control.Exception as Exception
import Control.Monad
import Control.Applicative
import Control.Arrow

import Foreign.Marshal.Alloc
import Foreign.Storable

#if __GLASGOW_HASKELL__ >= 610
catchAny f h = Exception.catch f (\e -> h (e :: Exception.SomeException))
#else
catchAny = Exception.catch
#endif

qachina :: IO ()
qachina = do
    -- putStrLn "Running qachina..."
    -- runCommand "mighty mighty.conf mighty.route" >>= waitForProcess
    return ()
    --pid <- runCommand "mighty mighty.conf mighty.route"
    --waitForProcess pid >>= exitWith

------------------------------------------------------------------------------

sxf = 0.0015  --手续费
yhs = 0.001   --印花费
ghf = 1.0     --过户费

--计算股票盈利
winG qty pb ps = qty * ps * (1 - sxf - yhs) - 2 * ghf - qty * pb * (1 + sxf)

--算权证盈利
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

fG y m d = fromGregorian y m d

no_trade_days = [fG 2011 1 1, fG 2011 1 2, fG 2011 1 3, --元旦
                 fG 2011 2 2, fG 2011 2 3, fG 2011 2 4, fG 2011 2 5, fG 2011 2 6, --春节
                 fG 2011 2 7, fG 2011 2 8,
                 fG 2011 4 3, fG 2011 4 4, fG 2011 4 5, --清明
                 fG 2011 4 30, fG 2011 5 1, fG 2011 5 2, --劳动
                 fG 2011 6 4, fG 2011 6 5, fG 2011 6 6,  --端午
                 fG 2013 9 19, fG 2013 9 20, fG 2013 9 21,  --中秋
                 fG 2011 10 1, fG 2011 10 2, fG 2011 10 3, fG 2011 10 4, fG 2011 10 5,  --国庆
                 fG 2011 10 6, fG 2011 10 7
                ]

is_trade_day day =
    if (elem wd [6,7]) || (elem day no_trade_days) then False else True
    where (y,w,wd) = toWeekDate day

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
find_the_day sDay 0 = sDay
find_the_day sDay nDay = 
    let nextDay = addDays 1 sDay
    in if (is_trade_day $ nextDay)
    then find_the_day nextDay (nDay - 1)
    else find_the_day nextDay nDay

------------------------------------------------------------------------
rollDice :: Int -> IO Int
rollDice n = do 
    tmp <- doesFileExist "/dev/urandom" 
    myGen <- if tmp
        then mkStdGen <$> betterSeed
        else (mkStdGen . fromInteger) <$> picoSec
    
    return $ (take 1 $ randomRs (1,n) myGen) !! 0

betterSeed :: IO Int
betterSeed = alloca $ \p -> do
    --h <- openBinaryFile "/dev/urandom" ReadMode
    withBinaryFile "/dev/urandom" ReadMode $ \h ->
        hGetBuf h p $ sizeOf (undefined :: Int)
    --hClose h
    peek p

picoSec :: IO Integer
picoSec = ctPicosec <$> (getClockTime >>= toCalendarTime)

------------------------------------------------------------------------

winSSQ count noRed noBlue = do
    --let noStr = (map (\x->show x) noRed) ++ ["-"] ++ (map (\x->show x) noBlue)
    --writeFile "noRedBlue.txt" $ concat $ intersperse " " noStr
    let noRedLst =  map (\x -> read x::Int) $ words noRed
        noBlueLst = map (\x -> read x::Int) $ words noBlue

    okBlue <- pickNums ([1..16] \\ noBlueLst) count []
    gRed <- goodRed
    result <- pickSSQ count gRed
              ([1..33] \\ noRedLst)
              okBlue []
    forM_ result (\x -> print x)
    writeFile ssqNum $ ints2str result

pickSSQ 0 _ _ _ acc = return acc
pickSSQ 1 gRed _ okBlue acc = do
    red <- sort <$> pickNums gRed 6 []
    return $ (red ++ [okBlue!!0]) : acc
pickSSQ count gRed yesRed okBlue acc = do
    red <- sort <$> pickNums yesRed 6 []
    pickSSQ (count-1) gRed yesRed okBlue $ 
        (red ++ [okBlue!!(count-1)]) : acc

ints2str :: [[Int]] -> String
ints2str ints = concat $ intersperse "\n" strLst
    where
        strLst = map (\x -> to_str x) ints
        to_str a = unwords $ map (\x -> show x) a 

goodRed:: IO [Int]
goodRed = do {
    samp <- fmap (concat . str_ints_hit) $ readFile ssqHitNum;
    return $ sort $ take 18 $ map (\(a,_) -> a) $ statis samp;
}

statis :: [Int] -> [(Int,Int)]
statis samp = map (\(a,b) -> (b,a)) $ times4n
    where
        times4n = (reverse . sort) $ map (length &&& head) $ (group . sort) samp

-- skip the 期号
str_ints_hit :: String -> [[Int]]
str_ints_hit str = map (\line -> map (\x->read x::Int) line) $ map (init . tail . words) $ lines str

str_ints_pick :: String -> [[Int]]
str_ints_pick str = map (\line -> map (\x->read x::Int) line) $ map words $ lines str

pickNums _ 0 acc = return acc
pickNums from count acc = do
    idx <-  rollDice $ length from
    pickNums (from \\ [from!!(idx-1)]) (count-1) (from!!(idx-1):acc)

hitSSQ :: String -> String -> IO ()
hitSSQ no hitNum = do
    let hitLst =  map (\x -> read x::Int) $ words hitNum
    let hitRed redNo = foldl (\acc x -> if(x `elem` (init hitLst)) then acc+1 else acc) 0 redNo

    dats <- readFile ssqHitNum
    if (any (\x -> no `elem` (words x)) (lines dats) == False)
        then catchAny (appendFile ssqHitNum $ unwords $ [no] ++ map (\n -> show n) hitLst ++ ["\n"]) (\e -> fail $ "Unable save hit numbers" ++ show e)
        else return ()

    gr <- goodRed
    
    appendFile "goodHitNums.txt" $ show (hitRed gr :: Int) ++ " "

    printf "Good Red Hit:%d of %d\n" (hitRed gr :: Int) (length gr)

    printf "------ result ------\n"
    nums <- fmap str_ints_pick $ readFile ssqNum
    forM_ nums (\n -> do
        let hitR = hitRed $ init n
            hitB = if (n!!6 == hitLst!!6) then 1 else 0 
        printf "%s\t%d:%d\t%s\n" (show n) (hitR::Int) (hitB::Int) (hit_desc hitR hitB))

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

ssqNum = "/media/D/qachina/db/doc/money/" ++ "ssqNum.txt"
ssqHitNum = "/media/D/qachina/db/doc/money/" ++ "ssqHitNum.txt"

------------------------------------------------------------------------
  
-- 菲波纳契数列
--fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)] 
fibs n = take n $ fibgen 1 1
fibgen n1 n2 = n1 : fibgen n2 (n1+n2) 

--求解素数的一个无限数列方法：
prime = sieve [2..]        
sieve (x:xs) = x : sieve (filter (\y ->y `rem` x /= 0) xs)

--排列组合 same as Data.List.subsequences
combination :: [a] -> [[a]]
combination [] =  [[]]
combination (x:xs) = concat [[(x:ys), ys] | ys <- combination xs] 

mcombs :: [a] -> [[a]]
mcombs = foldr (flip (>>=) . f) [[]]
    where
        f x xs = [x:xs,xs] 

permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = concatMap (\x -> map (x:) $ permutation (delete x xs)) xs

