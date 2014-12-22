module Main where

import Control.Concurrent.STM
import Control.Concurrent
import System.Random

main = do {
    elf_gp <- newGroup 3;
    sequence_ [ elf elf_gp n | n <- [1..10]];

    rein_gp <- newGroup 9;
    sequence_ [ reindeer rein_gp n | n <- [1..9]] ;

    forever (santa elf_gp rein_gp) }
  where
    elf      gp id = forkIO (forever (do { elf1 gp id; randomDelay }))
    reindeer gp id = forkIO (forever (do { reindeer1 gp id; randomDelay }))

santa :: Group -> Group -> IO ()
santa elf_group rein_group = do {
    putStr "----------\n" ;
    choose [(awaitGroup rein_group, run "deliver toys"), 
		    (awaitGroup elf_group,  run "meet in my study")] }
  where
    run :: String -> (Gate,Gate) -> IO ()
    run task (in_gate,out_gate) = do {
        putStr ("Ho! Ho! Ho! let's " ++ task ++ "\n") ;
	    operateGate in_gate;
	    operateGate out_gate }

helper1 :: Group -> IO () -> IO ()
helper1 group do_task = do {
    (in_gate, out_gate) <- joinGroup group;
    passGate in_gate;
    do_task;
    passGate out_gate }

elf1, reindeer1 :: Group -> Int -> IO ()
elf1      group id = helper1 group (meetInStudy id)
reindeer1 group id = helper1 group (deliverToys id)

meetInStudy id = putStr ("Elf " ++ show id ++ " meeting in the study\n")
deliverToys id = putStr ("Reindeer " ++ show id ++ " delivering toys\n")

---------------
data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup :: Int -> IO Group
newGroup n = atomically (
    do {
        g1 <- newGate n;
	    g2 <- newGate n;
	    tv <- newTVar (n, g1, g2);
        return (MkGroup n tv) })

joinGroup :: Group -> IO (Gate,Gate)
joinGroup (MkGroup n tv) = atomically (
    do {
        (n_left, g1, g2) <- readTVar tv;
       	check (n_left > 0);
       	writeTVar tv (n_left-1, g1, g2);
       	return (g1,g2) })

awaitGroup :: Group -> STM (Gate,Gate)
awaitGroup (MkGroup n tv) = do {
    (n_left, g1, g2) <- readTVar tv;
    check (n_left == 0);
    new_g1 <- newGate n;
    new_g2 <- newGate n;
    writeTVar tv (n,new_g1,new_g2);
    return (g1,g2) }

---------------
data Gate  = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do { tv <- newTVar 0; return (MkGate n tv) }

passGate :: Gate -> IO ()
passGate (MkGate n tv) = atomically (
    do { n_left <- readTVar tv;
  	     check (n_left > 0);
  	     writeTVar tv (n_left-1) })

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do {
    atomically (
        writeTVar tv n);
        atomically (
            do { n_left <- readTVar tv;
		         check (n_left == 0) }) }

----------------

forever :: IO () -> IO ()
-- Repeatedly perform the action
forever act = do { act; forever act }

randomDelay :: IO ()
-- Delay for a random time between 1 and 1000,000 microseconds
randomDelay = do { waitTime <- getStdRandom (randomR (1, 1000000));
                   threadDelay waitTime }

choose :: [(STM a, a -> IO ())] -> IO ()
choose choices = do {
    to_do <- atomically (foldr1 orElse stm_actions);
    to_do }
  where
    stm_actions :: [STM (IO ())]
    stm_actions = [ do { val <- guard; return (rhs val) }
		  | (guard, rhs) <- choices ] 

