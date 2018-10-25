cd ../lib/
rm -rf mdi_build
mkdir mdi_build
cd mdi_build
cmake -Dlanguage=C ../mdi
make
