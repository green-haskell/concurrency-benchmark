import Criterion.Main
import Program as D (exec)

main :: IO ()
main = defaultMain [
    bench "dining-philosophers (forkOn | TMVar)" $ nfIO (D.exec 64)
  ]
