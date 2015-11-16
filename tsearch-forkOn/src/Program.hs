module Program ( exec, exec' ) where

import Utils ( redirectStdOutAndRun )

import Control.Concurrent ( forkOn, forkFinally )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.SSem as Sem
import GHC.Conc ( numCapabilities )
import Data.IORef

import Scanner
import Buffer
import Engine
import Logger

exec :: String -> [String] -> IO ()
exec input queries = redirectStdOutAndRun (exec' input) queries

exec' :: String -> [String] -> IO ()
exec' path rawQueries  = do
    let nWorkers = numCapabilities
    let initialSubIndices = 4 :: Int
    let maxFiles = 3 -- max files processed per subindex

    capability <- newIORef 0

    loggerFinished <- atomically $ newEmptyTMVar
    logBuffer <- atomically newEmptyBuffer
    _ <- forkFinally (Logger.listen logBuffer) (\_ -> atomically $ putTMVar loggerFinished ())

    fileBuffer <- atomically newEmptyBuffer
    cap <- atomicModifyIORef' capability (\x -> (x + 1, x))
    _ <- forkOn cap $ Scanner.scan path fileBuffer

    queryIndexBuffer <- atomically newEmptyBuffer
    Engine.processFiles capability initialSubIndices maxFiles nWorkers fileBuffer queryIndexBuffer logBuffer

    finishSearch <- Engine.processSearch capability rawQueries queryIndexBuffer logBuffer

    atomically $ Sem.wait finishSearch
    Logger.finish logBuffer
    atomically $ takeTMVar loggerFinished
