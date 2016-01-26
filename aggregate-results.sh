#!/bin/bash

OUTPUT_DIR="results"

declare -A benchmark
benchmark["chameneos-redux1"]="forkIO-MVar;forkIO-TMVar;"
benchmark["chameneos-redux1"]+="forkOn-MVar;forkOn-TMVar;"
benchmark["dining-philosophers"]="forkIO-MVar;forkIO-TMVar;"
benchmark["dining-philosophers"]+="forkOn-MVar;forkOn-TMVar;"
benchmark["dining-philosophers"]+="forkOS-MVar;forkOS-TMVar"
benchmark["fasta7"]="forkIO-MVar;forkIO-TMVar;forkIO-TVar;"
benchmark["fasta7"]+="forkOn-MVar;forkOn-TMVar;forkOn-TVar;"
benchmark["fasta7"]+="forkOS-MVar;forkOS-TMVar;forkOS-TVar"
benchmark["k-nucleotide1"]="forkIO-MVar;forkIO-TMVar;"
benchmark["k-nucleotide1"]+="forkOn-MVar;forkOn-TMVar;"
benchmark["k-nucleotide1"]+="forkOS-MVar;forkOS-TMVar"
benchmark["mandelbrot2"]="forkIO-MVar;forkIO-TMVar;"
benchmark["mandelbrot2"]+="forkOn-MVar;forkOn-TMVar;"
benchmark["mandelbrot2"]+="forkOS-MVar;forkOS-TMVar"
benchmark["regex-dna2"]="forkIO-MVar;forkIO-TMVar;"
benchmark["regex-dna2"]+="forkOn-MVar;forkOn-TMVar;"
benchmark["regex-dna2"]+="forkOS-MVar;forkOS-TMVar"
benchmark["spectral-norm4"]="forkIO-MVar;forkIO-TMVar;forkIO-TVar;"
benchmark["spectral-norm4"]+="forkOn-MVar;forkOn-TMVar;forkOn-TVar;"
benchmark["spectral-norm4"]+="forkOS-MVar;forkOS-TMVar;forkOS-TVar"
benchmark["tsearch"]="forkIO;forkOn;forkOS"
benchmark["warp"]="forkIO;forkOn;forkOS"

rm -r $OUTPUT_DIR 2> /dev/null
mkdir $OUTPUT_DIR


pretty_num=($(seq -w 1 20))

for key in ${!benchmark[@]}; do
    index=0
    basedir=$OUTPUT_DIR/$key
    mkdir $basedir

    for kind in $(echo ${benchmark[${key}]} | tr ";" "\n");
    do
        benchdir=$key-$kind
        summary_file=$(ls $benchdir | grep summary | tail -1)
        cp $benchdir/$summary_file $basedir/${pretty_num[$index]}.$kind.txt
        cp -r $benchdir/out $basedir/raw-$kind
        let index++
    done

    cd $basedir
    python2 -c 'import os, json; print json.dumps(sorted(filter(lambda f: f.endswith(".txt"), os.listdir("."))))' >> files.json
    cd ../../
done

cd $OUTPUT_DIR
python2 -c 'import os, json; print json.dumps(sorted(filter(os.path.isdir, os.listdir("."))))' >> benchmarks.json
cd ../

cp summary-charts-template.html $OUTPUT_DIR/index.html

zip -r $OUTPUT_DIR.zip $OUTPUT_DIR
