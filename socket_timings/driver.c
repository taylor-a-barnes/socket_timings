#include <stdio.h>
#include <time.h>
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

  // Initialize the MDI driver
  //int ret = MDI_Init(8021);
  //int ret = MDI_Listen("TCP","8021",NULL);
  int ret = MDI_Init(argv[1], NULL);

  // Accept a communicator from the production code
  int comm = MDI_Accept_Communicator();

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
