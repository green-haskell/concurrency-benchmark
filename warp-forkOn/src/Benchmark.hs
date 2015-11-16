import Criterion.Main
import Client as C (exec)

main :: IO ()
main = defaultMain [
    bench "warp (forkOn)" $ nfIO C.exec
  ]
