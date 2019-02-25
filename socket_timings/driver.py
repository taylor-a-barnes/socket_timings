import sys
import time
import mdi.mdi as mdi
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

niterations = 10000
natoms = 10
get_coords = False

# initialize MPI
if use_mpi4py:
    mpi_world = MPI.COMM_WORLD
else:
    mpi_world = None

# get the Angstrom to Bohr conversion factor
conversion_factor = mdi.MDI_Conversion_Factor("Angstrom","Bohr")

# initialize the socket
mdi.MDI_Init(sys.argv[2],mpi_world)
if use_mpi4py:
    mpi_world = mdi.MDI_Get_Intra_Code_MPI_Comm()
    world_rank = mpi_world.Get_rank()
else:
    world_rank = 0

# connect to the production codes
ncodes = 1
if world_rank == 0:
    for icode in range(ncodes):
        comm = mdi.MDI_Accept_Communicator()

        # get the name of the code
        mdi.MDI_Send_Command("<NAME", comm)
        name = mdi.MDI_Recv(mdi.MDI_NAME_LENGTH, mdi.MDI_CHAR, comm)
        print('Received communicator: ' + str(name))

        if name.strip() == 'PONG' or name.strip() == "PONG_F90":
            mm_comm = comm
        else:
            raise ValueError('Production code name not recognized')


if world_rank == 0:

    initial_time = time.clock();

    for iiter in range(niterations):

        # get the MM energy
        mdi.MDI_Send_Command("<NAME", comm)
        name = mdi.MDI_Recv(mdi.MDI_NAME_LENGTH, mdi.MDI_CHAR, comm)

        # get the MM coordinates
        if get_coords:
            mdi.MDI_Send_Command("<COORDS", comm)
            if use_numpy:
                coords = mdi.MDI_Recv(3*natoms, mdi.MDI_DOUBLE_NUMPY, comm)
            else:
                coords = mdi.MDI_Recv(3*natoms, mdi.MDI_DOUBLE, comm)

        if iiter % 1000 == 0:
            print("Iteration: " + str(iiter) + "   " + name )


    # close the production codes
    mdi.MDI_Send_Command("EXIT", mm_comm)

    wall_time = time.clock() - initial_time
    if get_coords:
        print( str(coords) )
    print("Ping-pong time: " + str(wall_time))
    print("   us: " + str(1000000.0*wall_time/(1.0*(2*niterations))))
