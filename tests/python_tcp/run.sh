python ../../build/driver.py &
mpirun -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &

wait
