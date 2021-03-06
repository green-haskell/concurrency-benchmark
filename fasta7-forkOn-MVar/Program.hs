{-  The Computer Language Benchmarks Game

    http://benchmarkgame.alioth.debian.org/

    contributed by Bryan O'Sullivan
    parallelized by Maxim Sokolov based on
    go variant by Chris Bainbridge et al. 
-}

module Program (exec,main) where

import Utils(redirectStdOutAndRun)

import Control.Monad
import Control.Concurrent
import Data.ByteString.Unsafe
import qualified Data.ByteString.Internal as BI
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import GHC.Word
import Data.Char
import GHC.Conc (numCapabilities)

modulus = 139968

kWidth = 60
kLines = 1024
kBlockSize = kWidth * kLines
kNewLine = fromIntegral $ ord '\n'

main = do
    n <- getArgs >>= readIO.head
    exec' n

exec n = redirectStdOutAndRun exec' n
    
exec' n = do
    writeAlu ">ONE Homo sapiens alu" alu (fromIntegral n*2)
    make ">TWO IUB ambiguity codes" (n*3) iub 42 >>=
      void . make ">THREE Homo sapiens frequency" (n*5) homosapiens

writeAlu name s n = do
    B.putStrLn name
    let (!bufPtr, offset, _) = BI.toForeignPtr
                             $ L.toStrict
                             $ L.take (L.length s + fromIntegral kWidth)
                             $ L.cycle s
    withForeignPtr bufPtr $ \buf0 -> do
        let !sLen = fromIntegral $ L.length s
        let !buf = plusPtr buf0 offset
        let go _    !count | count == 0 = return ()
            go !pos !count | otherwise = do
                let !lineLen = min kWidth count
                hPutBuf stdout (plusPtr buf pos) lineLen
                B.putStrLn ""
                let !pos0 = pos + lineLen
                -- assuming kWidth <= sLen
                let !pos1 = if pos0 >= sLen then pos0 - sLen else pos0
                go pos1 (count - lineLen)
        go 0 n

make name n0 tbl seed0 = do
    B.putStrLn name
    let !lookupTable = buildLookupTable tbl
    ready <- newMVar ()
    input <- newMVar (seed0, ready, n0)
    finished <- newEmptyMVar
    forM_ [0..numCapabilities-1] $ \cpu -> forkOn cpu $ worker input finished lookupTable
    takeMVar finished

buildLookupTable tbl =
    let fill ((c,p):cps) j =
            let !k = min modulus (floor (fromIntegral modulus * (p::Float) + 1))
            in B.replicate (k - j) c : fill cps k
        fill _ _ = []
     in B.concat $ fill (scanl1 (\(_,p) (c,q) -> (c,p+q)) tbl) 0

worker input finished lookupTable = do
    rand <- mallocArray kBlockSize
    buf <- mallocArray (kBlockSize + kLines) :: IO (Ptr Word8)
    forever $ do
        (seed0, prevReady, count0) <- takeMVar input
        let !n = min kBlockSize count0
            !count1 = count0 - n
        seed1 <- fillRandomBlock n rand seed0
        ready <- newEmptyMVar
        when (count1 > 0) $ do
            putMVar input (seed1, ready, count1)
        k <- fillBuf n lookupTable rand buf
        takeMVar prevReady
        hPutBuf stdout buf k
        if count1 == 0
            then putMVar finished seed1
            else putMVar ready ()

fillRandomBlock !n0 !ptr !seed0 = do
    let go !j !seed
            | j < n0 = do
                    let newseed = rem (seed * 3877 + 29573) modulus
                    pokeElemOff ptr j newseed
                    go (j + 1) newseed
            | otherwise = return seed
    go 0 seed0

fillBuf n0 !lookupTable !rand !buf = do
    let go !i !j
            | i < n0 = do
                rnd <- peekElemOff rand i
                j1 <- if i > 0 && rem i kWidth == 0
                        then do
                            pokeElemOff buf j kNewLine
                            return $ j + 1
                        else return $ j
                pokeElemOff buf j1 (unsafeIndex lookupTable rnd)
                go (i+1) (j1+1)
            | otherwise = do
                pokeElemOff buf j kNewLine
                return (j+1)
    go 0 0

alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGG\
    \TCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGG\
    \CGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGC\
    \GGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub = [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
      ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
      ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homosapiens = [('a',0.3029549426680),('c',0.1979883004921)
              ,('g',0.1975473066391),('t',0.3015094502008)]
