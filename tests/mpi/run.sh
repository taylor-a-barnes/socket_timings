module unload intel/2017u4
module unload intel-mpi/2017u4

module load intel-python2/2017u3
module load intel/2017u4
module load intel-mpi/2017u4

# C - C
mpiexec -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method MPI" : \
    -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method MPI"

# C - Fortran
mpiexec -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method MPI" : \
    -n 2 ../../build/mpi_production_f90 -mdi "-role ENGINE -name MM -method MPI"

# C - Python
mpiexec -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method MPI" : \
    -n 2 python ../../build/production.py -mdi "-role ENGINE -name MM -method MPI"

# Fortran - C
mpiexec -n 2 ../../build/mpi_driver_f90 -mdi "-role DRIVER -name driver -method MPI" : \
    -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method MPI"

# Fortran - Fortran
mpiexec -n 2 ../../build/mpi_driver_f90 -mdi "-role DRIVER -name driver -method MPI" : \
    -n 2 ../../build/mpi_production_f90 -mdi "-role ENGINE -name MM -method MPI"

# Fortran - Python
mpiexec -n 2 ../../build/mpi_driver_f90 -mdi "-role DRIVER -name driver -method MPI" : \
    -n 2 python ../../build/production.py -mdi "-role ENGINE -name MM -method MPI"

# Python - C
mpiexec -n 2 python ../../build/driver.py -mdi "-role DRIVER -name driver -method MPI" : \
    -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method MPI"

# Python - Fortran
mpiexec -n 2 python ../../build/driver.py -mdi "-role DRIVER -name driver -method MPI" : \
    -n 2 ../../build/mpi_production_f90 -mdi "-role ENGINE -name MM -method MPI"

# Python - Python
mpiexec -n 2 python ../../build/driver.py -mdi "-role DRIVER -name driver -method MPI" : \
    -n 2 python ../../build/production.py -mdi "-role ENGINE -name MM -method MPI"
