module load intel/2017u4
module load intel-mpi/2017u4

#ensure that the intel compilers are being used
CLOCATION="$(which icc)"
FLOCATION="$(which ifort)"
export CC="${CLOCATION}"
export FC="${FLOCATION}"

cd ../lib/
rm -rf mdi_build
mkdir mdi_build
cd mdi_build
cmake ../mdi
make
cp ../mdi/molssi_driver_interface/mdi.h molssi_driver_interface
cd ../

cd ../
rm -rf build
mkdir build
cd build
cmake ../socket_timings
make