
# C - C
mpiexec -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method TCP -port 8021" &
mpiexec -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &
wait

# C - Fortran
mpiexec -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method TCP -port 8021" &
mpiexec -n 2 ../../build/mpi_production_f90 -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &
wait

# C - Python
mpiexec -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method TCP -port 8021" &
python ../../build/production.py -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &
wait



# Fortran - C
mpiexec -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method TCP -port 8021" &
mpiexec -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &
wait

# Fortran - Fortran
mpiexec -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method TCP -port 8021" &
mpiexec -n 2 ../../build/mpi_production_f90 -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &
wait

# Fortran - Python
mpiexec -n 2 ../../build/mpi_driver -mdi "-role DRIVER -name driver -method TCP -port 8021" &
python ../../build/production.py -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &
wait



# Python - C
python ../../build/driver.py -mdi "-role DRIVER -name driver -method TCP -port 8021" &
mpiexec -n 2 ../../build/mpi_production -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &
wait

# Python - Fortran
python ../../build/driver.py -mdi "-role DRIVER -name driver -method TCP -port 8021" &
mpiexec -n 2 ../../build/mpi_production_f90 -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &
wait

# Python - Python
python ../../build/driver.py -mdi "-role DRIVER -name driver -method TCP -port 8021" &
python ../../build/production.py -mdi "-role ENGINE -name MM -method TCP -hostname localhost -port 8021" &
wait
