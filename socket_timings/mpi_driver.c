#include <stdio.h>
#include <time.h>
#include <string.h>
#include <mpi.h>
#include "../lib/mdi_build/MDI_Library/mdi.h"

void mdi_ping_pong(int comm) {
  char message[MDI_NAME_LENGTH];

  MDI_Send_Command("<NAME",comm);
  MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, comm);

  //printf("NAME: %s\n",message);
}

int main(int argc, char **argv) {
  clock_t start, end;
  double cpu_time;
  int niter = 10000;
  int mpi_ptr;
  int world_rank;
  MPI_Comm world_comm;
  int i;

  // Initialize the MPI environment
  MPI_Init(&argc, &argv);

  // Ensure the mdi argument has been provided
  int iarg = 1;
  if ( !( argc-iarg >= 2 && strcmp(argv[iarg],"-mdi") == 0) ) {
    perror("The -mdi argument was not provided");
    return -1;
  }

  // Get the Angstrom-Bohr conversion factor
  double conversion_factor = MDI_Conversion_Factor("angstrom","bohr");
  printf("Conversion factor: %f\n",conversion_factor);

  // Initialize the MDI driver
  world_comm = MPI_COMM_WORLD;
  int ret = MDI_Init(argv[iarg+1], &world_comm);

  // Accept a communicator from the production code
  int comm = MDI_Accept_Communicator();

  // Note: For reasons I don't fully understand, the pointer returned by MPI
  // doesn't seem to persist throughout the test.
  // As a workaround, use mpi_ptr as the argument and then assign world_rank
  // to its value.
  MPI_Comm_rank(world_comm, &mpi_ptr);
  world_rank = mpi_ptr;

  start = clock();

  if ( world_rank == 0 ) {
    for (i=0; i<niter; i++) {
      mdi_ping_pong(comm);
      if (i%1000 == 0) {
	printf("Iteration: %i\n",i);
      }
    }
  }

  end = clock();
  cpu_time = ((double) (end - start)) / CLOCKS_PER_SEC;

  if ( world_rank == 0 ) {
    printf("Ping-pong time: %f\n",cpu_time);
    printf("   us: %f\n",1000000.0*cpu_time/((double) (2*niter)));
  }

  if ( world_rank == 0 ) {
    MDI_Send_Command("EXIT",comm);
  }

  MPI_Barrier(world_comm);

  MPI_Finalize();

  return 0;
}
