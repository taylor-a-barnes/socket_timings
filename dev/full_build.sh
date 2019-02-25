module load intel/2017u4
module load intel-mpi/2017u4

#ensure that the intel compilers are being used
CLOCATION="$(which icc)"
CXXLOCATION="$(which icc)"
FLOCATION="$(which ifort)"
export CC="${CLOCATION}"
export CXX="${CXXLOCATION}"
export FC="${FLOCATION}"

cd ../lib/
rm -rf mdi_build
mkdir mdi_build
cd mdi_build
cmake ../mdi
make
cp ../mdi/molssi_driver_interface/mdi.h molssi_driver_interface
cp ../mdi/molssi_driver_interface/mdi_global.h molssi_driver_interface
cd ../

rm -rf zmq_build
mkdir zmq_build
cd zmq_build
cmake ../libzmq
make -j 4
cd ../

cd ../
rm -rf build
mkdir build
cd build
cmake ../socket_timings
make
