import Criterion.Main
import Program as M (exec)

main :: IO ()
main = defaultMain [
    bench "mandelbrot #2 (forkOn | MVar)" $ nfIO (M.exec 4800)
  ]
