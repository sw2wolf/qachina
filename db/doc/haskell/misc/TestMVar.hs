module Main where

import Control.Concurrent.MVar
import Control.Concurrent
import Data.Maybe


timeout :: Int -> IO a -> IO (Maybe a)
timeout time action = do
  someMVar <- newEmptyMVar   -- MVar is a Maybe
  timeoutThread <- forkIO $ nothingIzer time someMVar
  forkIO $ actionRunner action someMVar timeoutThread
  takeMVar someMVar >>= return
    where 
      nothingIzer time mvar = threadDelay time >> putMVar mvar Nothing
      actionRunner action mvar timeoutThread = do
         res <- action
         killThread timeoutThread
         putMVar mvar $ Just res

main :: IO ()
main = do 
 res <- timeout (5 * 10 ^ 6) (getLine >>= putStrLn)
 case res of
    Nothing -> putStrLn "Timeout"
    Just x -> putStrLn "Success"
