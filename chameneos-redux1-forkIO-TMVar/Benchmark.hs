import Criterion.Main
import Program as C (exec)

main :: IO ()
main = defaultMain [
    bench "chameneos-redux #1 (TMVar)" $ nfIO (C.exec 60000000)
  ]
