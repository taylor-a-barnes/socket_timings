LD_LIBRARY_PATH=/home/tbarnes/mdi/stemp/lib/zmq_build/lib/ ../../build/zmq_server &

sleep 1

LD_LIBRARY_PATH=/home/tbarnes/mdi/stemp/lib/zmq_build/lib/ ../../build/zmq_client &

wait
