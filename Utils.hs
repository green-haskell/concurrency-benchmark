module Utils ( redirectStdOutAndRun
             , redirectStdIOAndRun ) where

import System.IO
import System.Directory (removeFile,getTemporaryDirectory)
import GHC.IO.Handle

redirectStdOutAndRun :: (t -> IO ()) -> t -> IO ()
redirectStdOutAndRun f n = do
    tmpDir <- getTemporaryDirectory
    (tmpFile, tmpHandle) <- openTempFile tmpDir "output.txt"
    oldStdOut <- hDuplicate stdout
    hDuplicateTo tmpHandle stdout
    f n
    hDuplicateTo oldStdOut stdout
    hClose tmpHandle
    removeFile tmpFile

redirectStdIOAndRun :: IO () -> String -> IO ()
redirectStdIOAndRun f input = do
    inputHandle <- openFile input ReadMode
    oldStdIn <- hDuplicate stdin
    hDuplicateTo inputHandle stdin

    tmpDir <- getTemporaryDirectory
    (tmpFile, tmpHandle) <- openTempFile tmpDir "output.txt"
    oldStdOut <- hDuplicate stdout
    hDuplicateTo tmpHandle stdout

    f

    hDuplicateTo oldStdIn stdin
    hClose inputHandle

    hDuplicateTo oldStdOut stdout
    hClose tmpHandle
    removeFile tmpFile
