module Client (exec,exec') where

import qualified Network.HTTP as H
import Control.Concurrent
import Control.Monad
import GHC.Conc
import Utils(redirectStdOutAndRun)

totalIterations = 500
numberOfRequests = 1000
numberOfThreads = numCapabilities

client :: MVar Int -> Int -> IO ()
client sem id = do
    let iterations = div totalIterations numberOfThreads
        oddIterations = totalIterations - iterations * numberOfThreads
        final = iterations + (if id <= oddIterations then 1 else 0)

    forM_ [1..final] $ \iter -> do
        forM_ [1..numberOfRequests] $ \task -> do
            rsp <- H.simpleHTTP (H.getRequest "http://127.0.0.1:3000")
            str <- H.getResponseBody rsp
            return ()

    putMVar sem 0

exec = redirectStdOutAndRun exec' 0

exec' _ = do
    vars <- replicateM numberOfThreads newEmptyMVar
    zipWithM_ (\mv id -> forkOS $ client mv id) vars [1..]
    mapM_ takeMVar vars
