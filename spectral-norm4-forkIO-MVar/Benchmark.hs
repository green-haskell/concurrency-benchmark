import Criterion.Main
import Program as S (exec)

main :: IO ()
main = defaultMain [
    bench "spectral-norm #4" $ nfIO (S.exec 10000)
  ]
