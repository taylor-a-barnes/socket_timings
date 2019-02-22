PROGRAM MDI_DRIVER_F90

USE mpi
USE ISO_C_binding
USE mdi,              ONLY : MDI_Init, MDI_Send, MDI_CHAR, MDI_NAME_LENGTH, &
     MDI_Accept_Communicator, MDI_Send_Command, MDI_Recv, MDI_MPI_COMM

IMPLICIT NONE

   INTEGER :: niter = 100000
   INTEGER :: mpi_ptr
   INTEGER :: world_comm, world_rank
   INTEGER :: i, ierr
   INTEGER :: comm_world, comm
   CHARACTER(len=:), ALLOCATABLE :: message
   CHARACTER(len=1024) :: arg
   CHARACTER(len=1024) :: mdi_options
   DOUBLE PRECISION :: initial_time, final_time

   ALLOCATE( character(MDI_NAME_LENGTH) :: message )

   ! Initialize the MPI environment
   call MPI_INIT(ierr)

   ! Get the command line arguments
   i = 0
   DO
      CALL get_command_argument(i, arg)
      IF (LEN_TRIM(arg) == 0) EXIT

      IF (TRIM(arg) .eq. "-mdi") THEN
         CALL get_command_argument(i+1, mdi_options)
         EXIT
      END IF

      i = i+1
   END DO

   ! Initialize the MDI driver
   world_comm = MPI_COMM_WORLD
   call MDI_Init( mdi_options, world_comm, ierr)

   ! Accept a communicator from the production code
   call MDI_Accept_Communicator(comm)

   !call MDI_MPI_Comm( world_comm, ierr )
   call MPI_Comm_rank( world_comm, mpi_ptr, ierr );
   world_rank = mpi_ptr

   CALL CPU_TIME(initial_time)
   DO i=1, niter
      IF( world_rank.eq.0 ) THEN
         call MDI_Send_Command("<NAME", comm, ierr)
         call MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, comm, ierr)

         !WRITE(6,*)'Iteration: ', i, message
         IF ( MODULO(i, 1000).eq.0 ) THEN
            WRITE(6,*)'Iteration: ',i,message
         END IF
      END IF
   END DO
   CALL CPU_TIME(final_time)

   IF( world_rank.eq.0 ) THEN
      WRITE(6,*)'Ping-pong time: ',final_time-initial_time
      WRITE(6,*)'   us: ',1000000.d0*(final_time-initial_time)/(DBLE(2*niter))
      call MDI_Send_Command("EXIT", comm, ierr)
   END IF

   call MPI_Barrier( world_comm, ierr )

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
