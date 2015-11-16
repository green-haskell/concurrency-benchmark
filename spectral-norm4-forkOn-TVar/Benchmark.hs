import Criterion.Main
import Program as S (exec)

main :: IO ()
main = defaultMain [
    bench "spectral-norm #4 (forkOn | TVar)" $ nfIO (S.exec 10000)
  ]
