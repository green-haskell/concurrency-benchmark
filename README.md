# concurrency-benchmark

## Instructions
### Install system dependencies
```
$ sudo add-apt-repository ppa:hvr/ghc
$ sudo apt-get update
$ sudo apt-get install ghc-7.10.2 cabal-install-1.22
$ sudo su
# export PATH=$PATH:/opt/ghc/7.10.2/bin:/opt/cabal/1.22/bin
# cabal update
# exit
$ sudo apt-get install libpcre3-dev zlib1g-dev
```

### Install Criterion with energy metrics
```
$ git clone https://github.com/green-haskell/criterion.git
$ cd criterion
$ sudo su
# export PATH=$PATH:/opt/ghc/7.10.2/bin:/opt/cabal/1.22/bin
# cabal install -g
# exit
```

### Clone repository and install benchmark dependencies
```
$ git clone https://github.com/green-haskell/concurrency-benchmark.git
$ cd concurrency-benchmark/
$ sudo su
# export PATH=$PATH:/opt/ghc/7.10.2/bin/:/opt/cabal/1.22/bin/
# cabal install -g regex-pcre
# cd warp-forkIO/
# cabal install -g --only-dependencies
# cd ../tsearch-forkIO/
# cabal install -g --only-dependencies
# exit
```

### Run the benchmarks
Once everything is setup, you can run each benchmark multiple times passing a different number of capabilities on each run by doing the following:

```
$ cd concurrency-benchmark/
$ ./generate-inputs.sh
$ sudo su
# export PATH=$PATH:/opt/ghc/7.10.2/bin/:/opt/cabal/1.22/bin/
# ./run-all.sh
```

You can configure the number of capabilities that are used by editing the `NCORES` array on `run-all.sh`.

You can also skip some benchmarks from the execution by including its directory name on the `blacklist.txt` file.

### Visualize the results
When the benchmarks finish executing, the results will be placed in a file called `DATE-bench-summary.txt` inside each benchmark directory. In order to aggregate and visualize it you can do the following:

```
$ sudo ./aggregate-results.sh
$ cd results/
$ python2 -m SimpleHTTPServer
Serving HTTP on 0.0.0.0 port 8000 ...
```

After doing this, you'll be able to access <http://localhost:8000/> in a web browser and see the results plotted in several charts like the ones [here](http://green-haskell.github.io/concurrency-results/).
