import sys
import mdi.mdi_python as mdi
try:
    from mpi4py import MPI
    use_mpi4py = True
except ImportError:
    use_mpi4py = False

niterations = 10

# initialize MPI
if use_mpi4py:
    mpi_world = MPI.COMM_WORLD
else:
    mpi_world = None

# initialize the socket
print( "BEFORE: " + str(mpi_world) + " " + str(mpi_world.Get_rank()) )
mdi.MDI_Init(sys.argv[2],None,mpi_world)
mpi_world = mdi.MDI_Get_Intra_Code_MPI_Comm()
print( "AFTER:  " + str(mpi_world) + " " + str(mpi_world.Get_rank()) )
world_rank = mpi_world.Get_rank()

# connect to the production codes
ncodes = 1
if world_rank == 0:
    for icode in range(ncodes):
        comm = mdi.MDI_Accept_Connection()

        # get the name of the code
        mdi.MDI_Send_Command("<NAME", comm)
        name = mdi.MDI_Recv(mdi.MDI_NAME_LENGTH, mdi.MDI_CHAR, comm)
        print('Received connection: ' + str(name))

        if name.strip() == 'PONG':
            mm_comm = comm
        else:
            raise ValueError('Production code name not recognized')


if world_rank == 0:
    for iiter in range(niterations):

        # get the MM energy
        mdi.MDI_Send_Command("<NAME", comm)
        name = mdi.MDI_Recv(mdi.MDI_NAME_LENGTH, mdi.MDI_CHAR, comm)

        print("Iteration: " + str(iiter+1) + "   " + name )


    # close the production codes
    mdi.MDI_Send_Command("EXIT", mm_comm)
