import Criterion.Main
import Program as R (exec)

main :: IO ()
main = defaultMain [
    bench "regex-dna #2 (forkOn | TMVar)" $ nfIO (R.exec "input.txt")
  ]
