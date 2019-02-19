module load intel-python2/2017u3

#mpiexec -n 2 python ../../build/driver.py -mdi "-role DRIVER -name driver -method MPI" : -n 2 python ../../build/driver.py -mdi "-role ENGINE -name MM -method MPI"
mpiexec -n 2 python ../../build/driver.py -mdi "-role DRIVER -name driver -method MPI" : -n 2 python ../../build/production.py -mdi "-role ENGINE -name MM -method MPI"

wait
