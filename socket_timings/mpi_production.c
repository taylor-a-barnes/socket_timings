#include <stdio.h>
#include <string.h>
#include <time.h>
#include <mpi.h>
#include "../lib/mdi_build/molssi_driver_interface/mdi.h"

int main() {
  clock_t start, end;
  double cpu_time;
  int mpi_ptr;
  int world_rank;
  MPI_Comm world_comm;

  // Initialize the MPI environment
  MPI_Init(NULL, NULL);

  // Initialize the MDI driver
  int comm = MDI_Request_Connection("MPI", "MM", NULL);

  // Note: For reasons I don't fully understand, the pointer returned by MPI
  // doesn't seem to persist throughout the test.
  // As a workaround, use mpi_ptr as the argument and then assign world_rank
  // to its value.
  MDI_MPI_Comm( &world_comm );
  MPI_Comm_rank(world_comm, &mpi_ptr);
  world_rank = mpi_ptr;

  start = clock();

  char command[MDI_COMMAND_LENGTH];
  while ( 1 ) {
    if ( world_rank == 0 ) {

      MDI_Recv_Command(command, comm);

      if( strcmp(command, "<NAME") == 0 ) {
	MDI_Send("PONG", MDI_NAME_LENGTH, MDI_CHAR, comm);
      }
      else if ( strcmp(command, "EXIT") == 0 ) {
	return 0;
      }
      else {
	perror("Error in MDI_Production: MDI command not recognized");
      }
    }
  }

  end = clock();
  cpu_time = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Ping-pong time: %f\n",cpu_time);
  
  return 0;
}
