ulimit -n 1280

./dist/build/server/server +RTS -N20 -RTS &

sleep 2

SERVER_PID=$!

./dist/build/client/client +RTS -N$1 -RTS >> /dev/null

kill $SERVER_PID
