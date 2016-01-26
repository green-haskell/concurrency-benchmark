#!/bin/bash
NCORES=(01 02 04 08 16 20 32 40 64)

if [[ $(/usr/bin/id -u) -ne 0 ]]; then
    echo "Hello, unprivileged user. One needs super user power here."
    exit 1
fi

modprobe msr

# set working directory = script's directory
# this is useful to run the script from ssh
DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd $DIR

for dir in */;
do
    if [ "x$dir" == "x`grep ${dir} blacklist.txt`" ]; then
        continue
    fi

    cd $dir

    make clean && ./configure.sh && make bench && mkdir -p out
    if [ $? -ne 0 ]; then
        echo
        echo "Failed to compile benchmark in \"$dir\". Skipping."
        echo
        cd ..
        continue
    fi

    for i in ${NCORES[*]};
    do
        echo
        echo "Running benchmark in \"$dir\" with N=$i: start"
        echo
        ./run-bench.sh $i > out/bench-$i.txt
        echo
        echo "Running benchmark in \"$dir\": done"
        echo
    done

    cat out/bench*.txt | sed -ne '/time\|iters/p' | sed 's/([^)]*)//g' | grep -o '[0-9]\+\.\?[0-9]*' | sed '$!N;s/\n/   /' >> `date '+%Y%m%d_%H%M%S'`-bench-summary.txt

    cd ..
done
