import Criterion.Main
import Client as C (exec)

main :: IO ()
main = defaultMain [
    bench "warp" $ nfIO C.exec
  ]
