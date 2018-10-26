module load intel/2017u4
module load intel-mpi/2017u4

cd ../lib/
rm -rf mdi_build
mkdir mdi_build
cd mdi_build
cmake -Dlanguage=C ../mdi
make
cp ../mdi/molssi_driver_interface/mdi.h molssi_driver_interface
cd ../

cd ../
rm -rf build
mkdir build
cd build
cmake ../socket_timings
make
