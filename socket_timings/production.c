#include <stdio.h>
#include <time.h>
#include "../lib/mdi_build/molssi_driver_interface/mdi.h"

int main() {
  clock_t start, end;
  double cpu_time;

  // Initialize the MDI driver
  //int comm = MDI_Open(1, 8021, "knl3.sirius.local.net");
  int comm = MDI_Request_Connection("TCP", "knl3.sirius.local.net:8021", NULL);

  start = clock();

  char command[MDI_COMMAND_LENGTH];
  while( strcmp(command, "EXIT") != 0 ) {
    MDI_Recv_Command(command, comm);

    if( strcmp(command, "<NAME") == 0 ) {
      MDI_Send("PONG       \0", MDI_NAME_LENGTH, MDI_CHAR, comm);
    }
  }

  end = clock();
  cpu_time = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Ping-pong time: %f\n",cpu_time);
  
  return 0;
}
