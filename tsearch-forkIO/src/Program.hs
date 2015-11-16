module Program ( exec, exec' ) where

import Utils ( redirectStdOutAndRun )

import Control.Concurrent ( forkIO, forkFinally )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.SSem as Sem
import GHC.Conc ( numCapabilities )

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

    loggerFinished <- atomically $ newEmptyTMVar
    logBuffer <- atomically newEmptyBuffer
    _ <- forkFinally (Logger.listen logBuffer) (\_ -> atomically $ putTMVar loggerFinished ())

    fileBuffer <- atomically newEmptyBuffer
    _ <- forkIO $ Scanner.scan path fileBuffer

    queryIndexBuffer <- atomically newEmptyBuffer
    Engine.processFiles initialSubIndices maxFiles nWorkers fileBuffer queryIndexBuffer logBuffer

    finishSearch <- Engine.processSearch rawQueries queryIndexBuffer logBuffer

    atomically $ Sem.wait finishSearch
    Logger.finish logBuffer
    atomically $ takeTMVar loggerFinished
