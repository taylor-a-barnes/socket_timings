import sys
#sys.path.insert(0, '../lib/mdi_build/molssi_driver_interface')
#sys.path.insert(0, '/Users/tbarnes/Documents/mdi/socket_timings/lib/mdi_build/molssi_driver_interface')

import mdi.mdi_python as mdi

niterations = 10

# initialize the socket
mdi.MDI_Init("-role DRIVER -name driver -method TCP -port 8021 -hostname localhost",None,None)

# connect to the production codes
ncodes = 1
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



for iiter in range(niterations):

    # get the MM energy
    mdi.MDI_Send_Command("<NAME", comm)
    name = mdi.MDI_Recv(mdi.MDI_NAME_LENGTH, mdi.MDI_CHAR, comm)

    print("Iteration: " + str(iiter+1) + "   " + name )


# close the production codes
mdi.MDI_Send_Command("EXIT", mm_comm)
