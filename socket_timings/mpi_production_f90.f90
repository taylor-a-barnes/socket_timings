PROGRAM MDI_DRIVER_F90

USE mpi
USE ISO_C_binding
USE mdi,              ONLY : MDI_Listen, MDI_Send, MDI_CHAR, MDI_NAME_LENGTH, &
     MDI_Open, MDI_Recv_Command, MDI_Recv, MDI_MPI

IMPLICIT NONE

   INTEGER :: ierr
   INTEGER :: comm_world, comm
   CHARACTER(len=:), ALLOCATABLE :: message

   ALLOCATE( character(MDI_NAME_LENGTH) :: message )

   ! Initialize the MPI environment
   call MPI_INIT(ierr)

   ! Initialize the MDI driver
   comm_world = MPI_COMM_WORLD
   call MDI_Open( comm, MDI_MPI, 0, "MM")

   ! respond to the driver's commands
   response_loop: DO

      call MDI_Recv_Command(message, comm, ierr)

      SELECT CASE( TRIM(message) )
      CASE( "<NAME" )
         call MDI_Send("PONG_F90", MDI_NAME_LENGTH, MDI_CHAR, comm, ierr)
      CASE( "<EXIT" )
         EXIT
      END SELECT

   END DO response_loop

END PROGRAM MDI_DRIVER_F90
