import Criterion.Main
import Program as K (exec)

main :: IO ()
main = defaultMain [
    bench "k-nucleotide #1" $ nfIO (K.exec "input.txt")
  ]