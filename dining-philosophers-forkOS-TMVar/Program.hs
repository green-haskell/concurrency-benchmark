{- 
 - http://rosettacode.org/wiki/Dining_philosophers#Haskell
 - -}

module Program (exec, main) where
import Utils(redirectStdOutAndRun)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import System.Environment

main = do
    n <- readIO . head =<< getArgs
    exec' n

exec n = redirectStdOutAndRun exec' n

exec' n = do
    forks <- mapM newFork [1..n]

    -- semaphore
    sem <- newTMVarIO 0

    let namedPhilosophers  = map (runPhilosopher sem) (philosophers n)
        forkPairs          = zip forks (tail . cycle $ forks)
        philosophersWithForks = zipWith ($) namedPhilosophers forkPairs

    mapM_ forkOS philosophersWithForks

    runMaybeT $ forever $ do
        s <- lift $ atomically $ readTMVar sem
        when (s == n) mzero
        -- wait some time, let the philosophers eat
        lift $ threadDelay 100000

    return ()

-- TMVars are transactional references. They can only be used in transactional actions.
-- They are either empty or contain one value. Taking an empty reference fails and
-- putting a value in a full reference fails. A transactional action only succeeds
-- when all the component actions succeed, else it rolls back and retries until it
-- succeeds.
-- The Int is just for display purposes.
type Fork = TMVar Int

newFork :: Int -> IO Fork
newFork i = newTMVarIO i

-- The basic transactional operations on forks
takeFork :: Fork -> STM Int
takeFork fork = takeTMVar fork

releaseFork :: Int -> Fork -> STM ()
releaseFork i fork = putTMVar fork i

type Name = String

runPhilosopher :: TMVar Int -> Name -> (Fork, Fork) -> IO ()
runPhilosopher sem name (left, right) = do
    forM_ [1..1000] $ \_ -> do
        --putStrLn (name ++ " is hungry.")

        -- Run the transactional action atomically.
        -- The type system ensures this is the only way to run transactional actions.
        (leftNum, rightNum) <- atomically $ do
            leftNum <- takeFork left
            rightNum <- takeFork right
            return (leftNum, rightNum)

        putStrLn (name ++ " got forks " ++ show leftNum ++ " and " ++ show rightNum ++ " and is now eating.")
        --threadDelay 100000 -- threadDelay uses nanoseconds.
        --putStrLn (name ++ " is done eating. Going back to thinking.")

        atomically $ do
            releaseFork leftNum left
            releaseFork rightNum right

        --threadDelay 100000

    atomically $ takeTMVar sem >>= (\s -> putTMVar sem (s + 1))

philosophers :: Int -> [String]
philosophers n = map (\x -> (name x) ++ (show $ number x)) [1..n]
    where
        names = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]
        name index = names !! (mod (index - 1) $ length names)
        number n = 1 + (div (n - 1) $ length names)
