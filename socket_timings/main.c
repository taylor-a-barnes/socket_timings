#include <mpi.h>
#include <stdio.h>

void mpi_ping_pong(int my_rank) {
  if(my_rank == 0) {
    char message[4] = "ping";

    MPI_Send(message, 4, MPI_CHAR, 1, 0, MPI_COMM_WORLD);

    printf("Head rank %s\n",message);
  }
  if(my_rank == 1) {
    char message[4] = "pong";
    MPI_Recv(message, 4, MPI_CHAR, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    printf("Received %s\n",message);
  }
}

int main() {
  // Initialize the MPI environment
  MPI_Init(NULL, NULL);

  // Get the number of processes
  int world_size;
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);

  // Get the rank of the process
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

  // Get the name of the processor
  char processor_name[MPI_MAX_PROCESSOR_NAME];
  int name_len;
  MPI_Get_processor_name(processor_name, &name_len);

  // Print off a hello world message
  printf("Hello world from processor %s, rank %d out of %d processors\n",
	 processor_name, world_rank, world_size);

  mpi_ping_pong(world_rank);

  // Finalize the MPI environment.
  MPI_Finalize();

  return 0;
}
