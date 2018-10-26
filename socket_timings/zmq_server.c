//  Hello World server
#include "../lib/libzmq/include/zmq.h"
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <time.h>

int main (void)
{
  clock_t start, end;
  double cpu_time;
  int niter = 100000;
  int i;

  //  Socket to talk to clients
  void *context = zmq_ctx_new ();
  void *responder = zmq_socket (context, ZMQ_REP);
  int rc = zmq_bind (responder, "tcp://*:5555");
  assert (rc == 0);

  start = clock();

  //while (1) {
  for (i=0; i<niter; i++) {
    char buffer [10];
    zmq_recv (responder, buffer, 10, 0);
    //printf ("Received Hello\n");
    //sleep (1);          //  Do some 'work'
    zmq_send (responder, "World", 5, 0);
  }

  end = clock();
  cpu_time = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Ping-pong time: %f\n",cpu_time);
  printf("   us: %f\n",1000000.0*cpu_time/((double) (2*niter)));

  return 0;
}
