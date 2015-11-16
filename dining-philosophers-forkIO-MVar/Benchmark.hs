import Criterion.Main
import Program as D (exec)

main :: IO ()
main = defaultMain [
    bench "dining-philosophers (forkIO | MVar)" $ nfIO (D.exec 64)
  ]
