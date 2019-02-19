import sys
import mdi.mdi_python as mdi
try:
    from mpi4py import MPI
    use_mpi4py = True
except ImportError:
    use_mpi4py = False

# initialize MPI
if use_mpi4py:
    mpi_world = MPI.COMM_WORLD
else:
    mpi_world = None

# initialize the socket
mdi.MDI_Init(sys.argv[2],None,mpi_world)
if use_mpi4py:
    mpi_world = mdi.MDI_Get_Intra_Code_MPI_Comm()
    world_rank = mpi_world.Get_rank()
else:
    world_rank = 0

# Accept the connection to the driver code
comm = mdi.MDI_Accept_Connection()

while True:
    if world_rank == 0:
        command = mdi.MDI_Recv_Command(comm)
    else:
        command = None
    command = mpi_world.bcast(command, root=0)

    if command == "<NAME":
        if world_rank == 0:
            mdi.MDI_Send("PONG", mdi.MDI_NAME_LENGTH, mdi.MDI_CHAR, comm)
    elif command == "EXIT":
        break
    else:
        raise Exception("Error in production.py: MDI command not recognized")

