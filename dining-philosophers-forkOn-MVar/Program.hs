{- 
 - http://rosettacode.org/wiki/Dining_philosophers#Haskell
 - -}

module Program (exec, main) where
import Utils(redirectStdOutAndRun)

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import System.Environment
import GHC.Conc

main = do
    n <- readIO . head =<< getArgs
    exec' n

exec n = redirectStdOutAndRun exec' n

exec' n = do
    forks <- mapM newFork [1..n]

    -- semaphore
    sem <- newMVar 0

    let namedPhilosophers     = map (runPhilosopher sem) (philosophers n)
        forkPairs             = zip forks (tail . cycle $ forks)
        philosophersWithForks = zipWith ($) namedPhilosophers forkPairs

    mapM_ (\(task, cap) -> forkOn cap task) (zip philosophersWithForks [0..])

    runMaybeT $ forever $ do
        s <- lift $ readMVar sem
        when (s == n) mzero
        -- wait some time, let the philosophers eat
        lift $ threadDelay 100000

    return ()

-- The Int is just for display purposes.
type Fork = MVar Int

newFork :: Int -> IO Fork
newFork i = newMVar i

-- The basic transactional operations on forks
takeFork :: Fork -> IO Int
takeFork fork = takeMVar fork

releaseFork :: Int -> Fork -> IO ()
releaseFork i fork = putMVar fork i

type Name = String

runPhilosopher :: MVar Int -> Name -> (Fork, Fork) -> IO ()
runPhilosopher sem name (left, right) = do
    forM_ [1..1000] $ \_ -> do
        --putStrLn (name ++ " is hungry.")

        leftNum <- takeFork left
        rightNum <- takeFork right

        putStrLn (name ++ " got forks " ++ show leftNum ++ " and " ++ show rightNum ++ " and is now eating.")
        --threadDelay (100000) -- threadDelay uses nanoseconds.
        --putStrLn (name ++ " is done eating. Going back to thinking.")

        releaseFork leftNum left
        releaseFork rightNum right

        --threadDelay (100000)

    takeMVar sem >>= (\s -> putMVar sem (s + 1))

philosophers :: Int -> [String]
philosophers n = map (\x -> (name x) ++ (show $ number x)) [1..n]
    where
        names = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]
        name index = names !! (mod (index - 1) $ length names)
        number n = 1 + (div (n - 1) $ length names)
