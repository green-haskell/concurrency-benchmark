import Criterion.Main
import Client as C (exec)

main :: IO ()
main = defaultMain [
    bench "warp (forkOS)" $ nfIO C.exec
  ]
