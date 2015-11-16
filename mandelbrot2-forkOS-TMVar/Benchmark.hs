import Criterion.Main
import Program as M (exec)

main :: IO ()
main = defaultMain [
    bench "mandelbrot #2 (forkOS | TMVar)" $ nfIO (M.exec 4800)
  ]
