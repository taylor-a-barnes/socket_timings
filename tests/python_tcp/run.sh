#python ../../build/driver.py &

python ../../build/driver.py -mdi "-role DRIVER -name driver -method TCP -port 8021" &
mpiexec -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &

wait
