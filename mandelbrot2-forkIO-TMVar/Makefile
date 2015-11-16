FLAGS = -XBangPatterns -threaded -rtsopts -optc-mfpmath=sse -optc-msse2

default: exec

exec:
	ghc --make $(FLAGS) -main-is Program.main Program.hs -o main

bench:
	ghc --make $(FLAGS) Benchmark.hs -o benchmark

clean:
	rm -f main benchmark *.aux *.hi *.hp *.o *.ps *.prof