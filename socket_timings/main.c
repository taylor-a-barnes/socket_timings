#include <mpi.h>
#include <stdio.h>
#include <time.h>

/*! \file
 *
 * \brief Pure MPI test.
 */

void mpi_ping_pong(int my_rank) {
  if(my_rank == 0) {
    char message[12] = "ping        ";

    //printf("Head rank send %s\n",message);

    MPI_Send(message, 12, MPI_CHAR, 1, 0, MPI_COMM_WORLD);
    MPI_Recv(message, 12, MPI_CHAR, 1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    //printf("Head rank recv %s\n",message);

  }
  if(my_rank == 1) {
    char rmessag[12];
    char message[12] = "pong        ";

    MPI_Recv(rmessag, 12, MPI_CHAR, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    MPI_Send(message, 12, MPI_CHAR, 0, 0, MPI_COMM_WORLD);

  }
}

int main() {
  clock_t start, end;
  double cpu_time;

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

  start = clock();

  int i;
  for (i=0; i<100000; i++) {
    mpi_ping_pong(world_rank);

    //if(world_rank == 0) {
    //  printf("MPI Iteration: %i\n",i);
    //}
  }

  end = clock();
  cpu_time = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Ping-pong time: %i %f\n",world_rank,cpu_time);
  

  // Finalize the MPI environment.
  MPI_Finalize();

  return 0;
}
