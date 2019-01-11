mpirun -n 2 ../../build/mpi_driver : -n 2 ../../build/mpi_production

mpirun -n 3 ../../build/mpi_driver : -n 16 ../../build/mpi_production_f90

mpirun -n 2 ../../build/mpi_driver_f90 : -n 16 ../../build/mpi_production

mpirun -n 15 ../../build/mpi_driver_f90 : -n 1 ../../build/mpi_production_f90

wait
