#include <stdio.h>
#include <time.h>
#include <mpi.h>
#include "../lib/mdi_build/molssi_driver_interface/mdi.h"

void mdi_ping_pong(int comm) {
  char message[MDI_NAME_LENGTH];

  MDI_Send_Command("<NAME",comm);
  MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, comm);

  //printf("NAME: %s\n",message);
}

int main() {
  clock_t start, end;
  double cpu_time;
  int niter = 100000;

  // Initialize the MPI environment
  MPI_Init(NULL, NULL);

  // Initialize the MDI driver
  int ret = MDI_Init_MPI();

  // Accept a connection from the production code
  int comm = MDI_Accept_Connection();

  start = clock();

  int i;
  for (i=0; i<niter; i++) {
    mdi_ping_pong(comm);
    if (i%1000 == 0) {
      printf("Iteration: %i\n",i);
    }
  }

  end = clock();
  cpu_time = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Ping-pong time: %f\n",cpu_time);
  printf("   us: %f\n",1000000.0*cpu_time/((double) (2*niter)));

  MDI_Send_Command("EXIT",comm);

  return 0;
}
