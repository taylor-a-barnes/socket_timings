#ensure that the intel compilers are being used
CLOCATION="$(which gcc)"
FLOCATION="$(which gfortran)"
export CC="${CLOCATION}"
export FC="${FLOCATION}"

cd ../lib/
rm -rf mdi_build
mkdir mdi_build
cd mdi_build
cmake -Dmpi=OFF ../mdi
make
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
make driver
make production
make mpi_driver
make mpi_production

