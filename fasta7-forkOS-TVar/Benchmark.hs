import Criterion.Main
import Program as F (exec)

main :: IO ()
main = defaultMain [
    bench "fasta #7 (forkOS | TVar)" $ nfIO (F.exec 200000000)
  ]
