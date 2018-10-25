#include <stdio.h>
#include <time.h>
#include "../lib/mdi_build/molssi_driver_interface/mdi.h"

void mdi_ping_pong(int comm) {
  char message[MDI_NAME_LENGTH];

  MDI_Send_Command("<NAME",comm);
  MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, comm);

  printf("NAME: %s\n",message);
}

int main() {
  clock_t start, end;
  double cpu_time;

  // Initialize the MDI driver
  int socket = MDI_Init(8021);

  // Accept a connection from the production code
  int comm = MDI_Accept_Connection(socket);

  start = clock();

  int i;
  for (i=0; i<100; i++) {
    mdi_ping_pong(comm);
  }

  end = clock();
  cpu_time = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Ping-pong time: %f\n",cpu_time);

  MDI_Send_Command("EXIT",comm);
  
  return 0;
}
