{- The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   Written by Tom Pledger, 13 Nov 2006. modified by Don Stewart
   Updated for chameneos-redux by Spencer Janssen, 27 Nov 2007
   Modified by Péter Diviánszky, 19 May 2010
   Modified by Louis Wasserman, 14 June 2010
   Modified by Iustin Pop, 30 September 2013:
     - moved releasing the mpv mvar to before updating the current and
       waiting chameleons; this is faster in single-core but slower in
       multi-core, unless -qa -qm are used
     - added recommendation to use -qa -qm, which makes the program an
       order of magnitude faster using GHC 7.6.3

   Should be compiled with --make -threaded -O2 and run with +RTS
     -N<number of cores> -qa -qm.
   -}

module Program (exec,main) where

import Utils(redirectStdOutAndRun)

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.IORef
import System.Environment
import System.IO
import GHC.Conc
import Foreign hiding (complement)

newtype Color = C Int deriving (Storable,Enum)

#define Y (C 2)
#define R (C 1)
#define B (C 0)

instance Show Color where
  show Y = "yellow"
  show R = "red"
  show B = "blue"

complement :: Color -> Color -> Color
complement !a !b = case a of
    B -> case b of R -> Y; B -> B; _ -> R
    R -> case b of B -> Y; R -> R; _ -> B
    Y -> case b of B -> R; Y -> Y; _ -> B

type Chameneous = Ptr Color
data MP = Nobody !Int | Somebody !Int !Chameneous !(MVar Chameneous)

arrive :: MVar MP -> MVar (Int, Int) -> Chameneous -> IO ()
arrive !mpv !finish !ch = do
    waker <- newEmptyMVar
    let inc x = (fromEnum (ch == x) +)
        go !t !b = do
            w <- takeMVar mpv
            case w of
                Nobody 0 -> do
                    putMVar mpv w
                    putMVar finish (t, b)
                Nobody q -> do
                    putMVar mpv $ Somebody q ch waker
                    ch' <- takeMVar waker
                    go (t+1) $ inc ch' b

                Somebody q ch' waker' -> do
                    let !q' = q-1
                    putMVar mpv $ Nobody q'
                    c  <- peek ch
                    c' <- peek ch'
                    let !c'' = complement c c'
                    poke ch  c''
                    poke ch' c''
                    putMVar waker' ch
                    go (t+1) $ inc ch' b
    go 0 0

showN = unwords . map ((digits !!) . digitToInt) . show

digits = words "zero one two three four five six seven eight nine"

run :: Int -> Int -> [Color] -> IO (IO ())
run n cpu cs = do
    fs    <- replicateM (length cs) newEmptyMVar
    mpv   <- newMVar (Nobody n)
    withArrayLen cs $ \ n cols -> do
        zipWithM_ ((forkIO .) . arrive mpv) fs (take n (iterate (`advancePtr` 1) cols))

        return $ do
          putStrLn . map toLower . unwords . ([]:) . map show $ cs
          ns    <- mapM takeMVar fs
          putStr . map toLower . unlines $ [unwords [show n, showN b] | (n, b) <- ns]
          putStrLn . (" "++) . showN . sum . map fst $ ns
          putStrLn ""

exec' n = do
    putStrLn . map toLower . unlines $
        [unwords [show a, "+", show b, "->", show $ complement a b]
            | a <- [B..Y], b <- [B..Y]]

    actions <- zipWithM (run n) [0..] [[B..Y],[B,R,Y,R,Y,B,R,Y,R,B]]
    sequence_ actions

main = do
    n <- readIO . head =<< getArgs
    exec' n

exec n = redirectStdOutAndRun exec' n
