import Criterion.Main
import Program as S (exec)

main :: IO ()
main = defaultMain [
    bench "spectral-norm #4 (TVar)" $ nfIO (S.exec 10000)
  ]
