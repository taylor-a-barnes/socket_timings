PROGRAM MDI_DRIVER_F90

USE mpi
USE ISO_C_binding
USE mdi,              ONLY : MDI_Send, MDI_CHAR, MDI_NAME_LENGTH, &
     MDI_Accept_Communicator, MDI_Recv_Command, MDI_Recv, MDI_MPI, MDI_Init

IMPLICIT NONE

   INTEGER :: i, ierr
   INTEGER :: mpi_ptr
   INTEGER :: world_comm, world_rank
   INTEGER :: comm
   CHARACTER(len=:), ALLOCATABLE :: message
   CHARACTER(len=1024) :: arg
   CHARACTER(len=1024) :: mdi_options

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

   ! Accept the communicator to the production code
   call MDI_Accept_Communicator(comm)

   call MPI_Comm_rank( world_comm, mpi_ptr, ierr );
   world_rank = mpi_ptr

   ! respond to the driver's commands
   response_loop: DO

      IF( world_rank.eq.0 )THEN
         call MDI_Recv_Command(message, comm, ierr)

         SELECT CASE( TRIM(message) )
         CASE( "<NAME" )
            call MDI_Send("PONG_F90", MDI_NAME_LENGTH, MDI_CHAR, comm, ierr)
         CASE( "EXIT" )
            EXIT
         END SELECT
      END IF

   END DO response_loop

END PROGRAM MDI_DRIVER_F90
