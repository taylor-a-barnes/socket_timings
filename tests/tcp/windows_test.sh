
echo "Running Windows Tests"

# C - C
..\..\build\mpi_driver -mdi "-role DRIVER -name driver -method TCP -port 8021" &
..\..\build\mpi_production -mdi "-role ENGINE -name PONG -method TCP -hostname localhost -port 8021" &
wait
