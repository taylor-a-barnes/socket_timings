import sys
import mdi.mdi_python as mdi
try:
    import numpy
    use_numpy = True
except ImportError:
    use_numpy = False
try:
    from mpi4py import MPI
    use_mpi4py = True
except ImportError:
    use_mpi4py = False

natoms = 10

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

coords = [ 0.1*icoord for icoord in xrange(3*natoms) ]
if use_numpy:
    numpy_coords = numpy.array( coords, dtype='float64' )

while True:
    if world_rank == 0:
        command = mdi.MDI_Recv_Command(comm)
    else:
        command = None
    if use_mpi4py:
        command = mpi_world.bcast(command, root=0)

    if command == "<NAME":
        if world_rank == 0:
            mdi.MDI_Send("PONG", mdi.MDI_NAME_LENGTH, mdi.MDI_CHAR, comm)
    elif command == "<COORDS":
        if world_rank == 0:
            if use_numpy:
                mdi.MDI_Send(numpy_coords, 3*natoms, mdi.MDI_DOUBLE_NUMPY, comm)
            else:
                mdi.MDI_Send(coords, 3*natoms, mdi.MDI_DOUBLE, comm)
    elif command == "EXIT":
        break
    else:
        raise Exception("Error in production.py: MDI command not recognized")

