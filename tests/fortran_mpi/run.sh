mpirun -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method MPI" : -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method MPI"

mpirun -n 3 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method MPI" : -n 16 ../../build/mpi_production_f90 -mdi "-role ENGINE -name MM -method MPI"

mpirun -n 2 ../../build/mpi_driver_f90 -mdi "-role DRIVER -name driver -method MPI" : -n 16 ../../build/mpi_production -mdi "-role ENGINE -name MM -method MPI"

mpirun -n 15 ../../build/mpi_driver_f90 -mdi "-role DRIVER -name driver -method MPI" : -n 1 ../../build/mpi_production_f90 -mdi "-role ENGINE -name MM -method MPI"

wait
