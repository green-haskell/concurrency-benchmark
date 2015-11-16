import Criterion.Main
import Program as T ( exec )

main :: IO ()
main = defaultMain [
    bench "tsearch (forkOn)" $ nfIO (T.exec "data/" ["same way you"])
  ]
