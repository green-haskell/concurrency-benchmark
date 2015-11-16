FLAGS = -O2 -XBangPatterns -threaded -rtsopts -funbox-strict-fields

default: exec

exec:
	ghc --make $(FLAGS) -main-is Program.main Program.hs -o main

bench:
	ghc --make $(FLAGS) Benchmark.hs -o benchmark

clean:
	rm -f main benchmark *.aux *.hi *.hp *.o *.ps *.prof