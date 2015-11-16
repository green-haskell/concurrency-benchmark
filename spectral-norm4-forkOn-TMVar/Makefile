FLAGS = -O2 -XBangPatterns -threaded -rtsopts -XMagicHash -funbox-strict-fields -optc-O3 -fexcess-precision -optc-ffast-math -fno-warn-tabs

default: exec

exec:
	ghc --make $(FLAGS) -main-is Program.main Program.hs -o main

bench:
	ghc --make $(FLAGS) Benchmark.hs -o benchmark

clean:
	rm -f main benchmark *.aux *.hi *.hp *.o *.ps *.prof
