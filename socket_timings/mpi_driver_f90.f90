PROGRAM MDI_DRIVER_F90

USE mpi
USE ISO_C_binding
USE mdi,              ONLY : MDI_Listen, MDI_Send, MDI_CHAR, MDI_NAME_LENGTH, &
     MDI_Accept_Connection, MDI_Send_Command, MDI_Recv

IMPLICIT NONE

   INTEGER :: niter = 100
   INTEGER :: i, ierr
   INTEGER :: comm_world, comm
   CHARACTER(len=:), ALLOCATABLE :: message

   ALLOCATE( character(MDI_NAME_LENGTH) :: message )

   ! Initialize the MPI environment
   call MPI_INIT(ierr)

   ! Initialize the MDI driver
   comm_world = MPI_COMM_WORLD
   call MDI_Listen( "MPI", c_null_ptr, comm_world, ierr)

   ! Accept a connection from the production code
   call MDI_Accept_Connection(comm)

   DO i=1, niter
      call MDI_Send_Command("<NAME", comm, ierr)
      call MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, comm, ierr)

      WRITE(6,*)'Iteration: ', i, message
   END DO

END PROGRAM MDI_DRIVER_F90



!void mdi_ping_pong(int comm) {
!  char message[MDI_NAME_LENGTH];
!
!  MDI_Send_Command("<NAME",comm);
!  MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, comm);
!
!  //printf("NAME: %s\n",message);
!}
!
!int main() {
!  clock_t start, end;
!  double cpu_time;
!  int niter = 100000;
!
!  // Initialize the MPI environment
!  MPI_Init(NULL, NULL);
!
!  // Initialize the MDI driver
!  //int ret = MDI_Init_MPI();
!  int ret = MDI_Listen("MPI", NULL, NULL);
!
!  // Accept a connection from the production code
!  int comm = MDI_Accept_Connection();
!
!  start = clock();
!
!  int i;
!  for (i=0; i<niter; i++) {
!    mdi_ping_pong(comm);
!    if (i%1000 == 0) {
!      printf("Iteration: %i\n",i);
!    }
!  }
!
!  end = clock();
!  cpu_time = ((double) (end - start)) / CLOCKS_PER_SEC;
!  printf("Ping-pong time: %f\n",cpu_time);
!  printf("   us: %f\n",1000000.0*cpu_time/((double) (2*niter)));
!
!  MDI_Send_Command("EXIT",comm);
!
!  return 0;
!}
