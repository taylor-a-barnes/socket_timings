PROGRAM MDI_DRIVER_F90

USE mpi
USE ISO_C_binding
USE mdi,              ONLY : MDI_Listen, MDI_Send, MDI_CHAR, MDI_NAME_LENGTH, &
     MDI_Request_Connection, MDI_Recv_Command, MDI_Recv, MDI_MPI, MDI_MPI_COMM

IMPLICIT NONE

   INTEGER :: ierr
   INTEGER :: mpi_ptr
   INTEGER :: world_comm, world_rank
   INTEGER :: comm
   CHARACTER(len=:), ALLOCATABLE :: message

   ALLOCATE( character(MDI_NAME_LENGTH) :: message )

   ! Initialize the MPI environment
   call MPI_INIT(ierr)

   ! Initialize the MDI driver
   call MDI_Request_Connection( "MPI", "MM", MPI_COMM_WORLD, comm )

   call MDI_MPI_Comm( world_comm, ierr )
   call MPI_Comm_rank( world_comm, mpi_ptr, ierr );
   world_rank = mpi_ptr

   ! respond to the driver's commands
   response_loop: DO

      IF( world_rank.eq.0 )THEN
         call MDI_Recv_Command(message, comm, ierr)

         SELECT CASE( TRIM(message) )
         CASE( "<NAME" )
            call MDI_Send("PONG_F90", MDI_NAME_LENGTH, MDI_CHAR, comm, ierr)
         CASE( "<EXIT" )
            EXIT
         END SELECT
      END IF

   END DO response_loop

END PROGRAM MDI_DRIVER_F90
