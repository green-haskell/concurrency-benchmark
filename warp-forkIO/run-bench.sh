ulimit -n 1280

./dist/build/server/server +RTS -N20 -RTS &

sleep 2

SERVER_PID=$!

./dist/build/benchmark/benchmark -v 2 --regress energy:iters +RTS -N$1 -RTS

kill $SERVER_PID
