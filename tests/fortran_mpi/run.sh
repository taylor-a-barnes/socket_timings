mpirun -n 1 ../../build/mpi_driver : -n 1 ../../build/mpi_production

mpirun -n 1 ../../build/mpi_driver : -n 1 ../../build/mpi_production_f90

mpirun -n 1 ../../build/mpi_driver_f90 : -n 1 ../../build/mpi_production

mpirun -n 1 ../../build/mpi_driver_f90 : -n 1 ../../build/mpi_production_f90

wait
