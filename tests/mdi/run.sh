mpirun -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method TCP -port 8021" &
mpirun -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &

wait
