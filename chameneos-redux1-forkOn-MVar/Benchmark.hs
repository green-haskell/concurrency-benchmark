import Criterion.Main
import Program as C (exec)

main :: IO ()
main = defaultMain [
    bench "chameneos-redux #1" $ nfIO (C.exec 60000000)
  ]
