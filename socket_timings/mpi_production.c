#include <stdio.h>
#include <string.h>
#include <time.h>
#include <mpi.h>
#include "../lib/mdi_build/molssi_driver_interface/mdi.h"

int main() {
  clock_t start, end;
  double cpu_time;

  // Initialize the MPI environment
  MPI_Init(NULL, NULL);

  // Initialize the MDI driver
  //int comm = MDI_Open(MDI_MPI, 0, "MM");
  int comm = MDI_Request_Connection("MPI", "MM", NULL);

  start = clock();

  char command[MDI_COMMAND_LENGTH];
  while ( 1 ) {
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

  end = clock();
  cpu_time = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Ping-pong time: %f\n",cpu_time);
  
  return 0;
}
