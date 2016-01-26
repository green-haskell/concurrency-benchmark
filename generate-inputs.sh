#!/bin/bash

cd fasta7-forkIO-MVar/

make clean
./configure.sh
make
./main +RTS -N4 -RTS 25000000 > input.txt

echo ../k-nucleotide1*/ | xargs -n 1 cp input.txt
echo ../regex-dna2*/ | xargs -n 1 cp input.txt

echo "> INPUT GENERATION FINISHED!"
