LD_LIBRARY_PATH=../../lib/zmq_build/lib/ ../../build/zmq_server &

sleep 1

LD_LIBRARY_PATH=../../lib/zmq_build/lib/ ../../build/zmq_client &

wait
