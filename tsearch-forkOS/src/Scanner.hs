module Scanner ( scan ) where

import System.Directory ( getDirectoryContents, doesFileExist, doesDirectoryExist )
import System.FilePath
import Control.Monad ( filterM, forM_ )
import Control.Concurrent.STM
import Buffer

getDirectoryEntries :: FilePath -> IO ([FilePath], [FilePath])
getDirectoryEntries path = do
    entries <- getDirectoryContents path
    let filtered = [path </> e | e <- entries, e `notElem` [".", ".."]]
    files <- filterM doesFileExist filtered
    dirs <- filterM doesDirectoryExist filtered
    return (files, dirs)

scan' :: FilePath -> Buffer FilePath -> IO ()
scan' dirPath buffer = do
    (files, dirs) <- getDirectoryEntries dirPath
    forM_ files $ atomically . (writeBuffer buffer)
    forM_ dirs $ flip scan' buffer

scan :: FilePath -> Buffer FilePath -> IO ()
scan dirPath buffer = do
    scan' dirPath buffer
    atomically $ enableFlag buffer
