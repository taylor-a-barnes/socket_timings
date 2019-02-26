/*! \file
 *
 * \brief Functions callable by users of the MolSSI Driver Interface
 */



/* ----------------------------------------------------------------------
   MDI - MolSSI Driver Interface
   https://molssi.org/, Molecular Sciences Software Institute
   Taylor Barnes, tbarnes1@vt.edu
-------------------------------------------------------------------------

Contents:
   MDI_Init: Initialize MDI
   MDI_Accept_Communicator: Accepts a new MDI communicator
   MDI_Send: Sends data through the socket
   MDI_Recv: Receives data through the socket
   MDI_Send_Command: Sends a string of length MDI_COMMAND_LENGTH through the
      socket
   MDI_Recv_Command: Receives a string of length MDI_COMMAND_LENGTH through the
      socket
*/

#include <signal.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include <iostream>
#include <vector>
#include "mdi.h"
#include "communicator.h"
#include "mdi_manager.h"
#include "method.h"

using namespace MDI_STUBS;
using namespace std;



// length of an MDI command in characters
const int MDI_COMMAND_LENGTH = 12;

// length of an MDI name in characters
const int MDI_NAME_LENGTH = 12;

// value of a null communicator
const MDI_Comm MDI_NULL_COMM = 0;

// MDI data types
const int MDI_INT          = 0;
const int MDI_DOUBLE       = 1;
const int MDI_CHAR         = 2;
const int MDI_INT_NUMPY    = 3;
const int MDI_DOUBLE_NUMPY = 4;

// MDI communication types
const int MDI_TCP    = 1;
const int MDI_MPI    = 2;

/*----------------------*/
/* MDI unit conversions */
/*----------------------*/

// length
const double MDI_METER_TO_BOHR = 1.88972612546e10;
const double MDI_ANGSTROM_TO_BOHR = 1.88972612546;

// time
const double MDI_SECOND_TO_AUT = 4.1341374575751e16;
const double MDI_PICOSECOND_TO_AUT = 4.1341374575751e4;

// force
const double MDI_NEWTON_TO_AUF = 1.213780478e7;

// energy
const double MDI_JOULE_TO_HARTREE = 2.29371265835792e17;
const double MDI_KJ_TO_HARTREE = 2.29371265835792e20;
const double MDI_KJPERMOL_TO_HARTREE = 3.80879947807451e-4;
const double MDI_KCALPERMOL_TO_HARTREE = 1.5941730215480900e-3;
const double MDI_EV_TO_HARTREE = 3.67493266806491e-2;
const double MDI_RYDBERG_TO_HARTREE = 0.5;
const double MDI_KELVIN_TO_HARTREE = 3.16681050847798e-6;


static MDIManager* manager;

static MethodMPI* method_mpi;



void mdi_error(const char* message) {
  perror(message);
  exit(1);
}


/*----------------------------*/
/* Signal handler definitions */
/*----------------------------*/

int driver_sockfd;
void sigint_handler(int dummy) {
  close(driver_sockfd);
}


int MDI_Listen_TCP(int port)
{
  int ret;
  int sockfd;
  struct sockaddr_in serv_addr;
  int reuse_value = 1;

  // create the socket
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    perror("Could not create socket");
    return -1;
  }

  // ensure that the socket is closed on sigint
  driver_sockfd = sockfd;
  signal(SIGINT, sigint_handler);

  // create the socket address
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port = htons(port);

  // enable reuse of the socket
  ret = setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &reuse_value, sizeof(int));
  if (ret < 0) {
    perror("Could not reuse socket");
    return -1;
  }

  // bind the socket
  ret = bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr));
  if (ret < 0) {
    perror("Could not bind socket");
    return -1;
  }

  // start listening (the second argument is the backlog size)
  ret = listen(sockfd, 20);
  if (ret < 0) {
    perror("Could not listen");
    return -1;
  }

  //return sockfd;
  manager->tcp_socket = sockfd;

  return 0;
}


int MDI_Request_Connection_TCP(int port, char* hostname_ptr)
{
  int ret, sockfd;

  struct sockaddr_in driver_address;
  struct hostent* host_ptr;

  // get the address of the host
  host_ptr = gethostbyname((char*) hostname_ptr);
  if (host_ptr == NULL) {
    perror("Error in gethostbyname");
    return -1;
  }
  if (host_ptr->h_addrtype != AF_INET) {
    perror("Unkown address type");
    return -1;
  }

  bzero((char *) &driver_address, sizeof(driver_address));
  driver_address.sin_family = AF_INET;
  driver_address.sin_addr.s_addr = 
    ((struct in_addr *)host_ptr->h_addr_list[0])->s_addr;
  driver_address.sin_port = htons(port);

  // create the socket
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    perror("Could not create socket");
    return -1;
  }

  // connect to the driver
  // if the connection is refused, try again
  //   this allows the production code to start before the driver
  int try_connect = 1;
  while (try_connect == 1) {
    ret = connect(sockfd, (const struct sockaddr *) &driver_address, sizeof(struct sockaddr));
    if (ret < 0 ) {
      if ( errno == ECONNREFUSED ) {

	// close the socket, so that a new one can be created
	ret = close(sockfd);
	if (ret < 0) {
	  perror("Could not close socket");
	  return -1;
	}

	// create the socket
	sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd < 0) {
	  perror("Could not create socket");
	  return -1;
	}

      }
      else { // only error out for errors other than "connection refused"
	perror("Could not connect to the driver");
	return -1;
      }

    }
    else {
      try_connect = 0;
    }
  }

  Communicator* new_communicator = new CommunicatorTCP( MDI_TCP, sockfd );

  return 0;
}




/*--------------------------*/
/* MDI function definitions */
/*--------------------------*/


/*! \brief Initialize communication through the MDI library
 *
 * If using the "-method MPI" option, this function must be called by all ranks.
 * The function returns \p 0 on a success.
 *
 * \param [in]       options
 *                   Options describing the communication method used to connect to codes
 * \param [in, out]  world_comm
 *                   On input, the MPI communicator that spans all of the codes.
 *                   On output, the MPI communicator that spans the single code corresponding to the calling rank.
 *                   Only used if the "-method MPI" option is provided.
 */
int MDI_Init(const char* options, void* world_comm)
{
  manager = new MDIManager();
  method_mpi = new MethodMPI();

  int ret;
  int sockfd;
  struct sockaddr_in serv_addr;
  int reuse_value = 1;
  char* strtol_ptr;
  int i;

  bool mpi_initialized = false;

  // values acquired from the input options
  char* role;
  char* method;
  char* name;
  char* hostname;
  int port;
  char* language;
  int has_role = 0;
  int has_method = 0;
  int has_name = 0;
  int has_hostname = 0;
  int has_port = 0;
  int has_language;

  // get the MPI rank
  MPI_Comm mpi_communicator;
  int mpi_rank = 0;
  if ( world_comm == NULL ) {
    mpi_communicator = 0;
    mpi_rank = 0;
  }
  else {
    mpi_communicator = *(MPI_Comm*) world_comm;
    MPI_Comm_rank(mpi_communicator, &mpi_rank);
  }

  // calculate argc
  char* argv_line = strdup(options);
  char* token = strtok(argv_line, " ");
  int argc = 0;
  while (token != NULL) {
    argc++;
    token = strtok(NULL," ");
  }

  // calculate argv
  char* argv[argc];
  argv_line = strdup(options);
  token = strtok(argv_line, " ");
  for (i=0; i<argc; i++) {
    argv[i] = token;
    token = strtok(NULL," ");
  }

  // read options
  int iarg = 0;
  while (iarg < argc) {

    //-role
    if (strcmp(argv[iarg],"-role") == 0){
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -role option");
      }
      role = argv[iarg+1];
      has_role = 1;
      iarg += 2;
    }
    //-method
    else if (strcmp(argv[iarg],"-method") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -method option");
      }
      method = argv[iarg+1];
      has_method = 1;
      iarg += 2;
    }
    //-name
    else if (strcmp(argv[iarg],"-name") == 0){
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -name option");
      }
      name = argv[iarg+1];
      has_name = 1;
      iarg += 2;
    }
    //-hostname
    else if (strcmp(argv[iarg],"-hostname") == 0){
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -hostname option");
      }
      hostname = argv[iarg+1];
      has_hostname = 1;
      iarg += 2;
    }
    //-port
    else if (strcmp(argv[iarg],"-port") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -port option");
      }
      port = strtol( argv[iarg+1], &strtol_ptr, 10 );
      has_port = 1;
      iarg += 2;
    }
    //_language
    else if (strcmp(argv[iarg],"_language") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Argument missing from _language option");
      }
      language = argv[iarg+1];
      has_language = 1;
      iarg += 2;
    }
    else {
      mdi_error("Unrecognized option");
    }
  }

  // ensure the -role option was provided
  if ( has_role == 0 ) {
    mdi_error("Error in MDI_Init: -role option not provided");
  }

  // ensure the -name option was provided
  if ( has_name == 0 ) {
    mdi_error("Error in MDI_Init: -name option not provided");
  }

  // determine whether the intra-code MPI communicator should be split by gather_names
  bool do_split = true;
  if ( strcmp(language, "Python") == 0 ) {
    do_split = false;
  }



  if ( strcmp(role, "DRIVER") == 0 ) {
    // initialize this code as a driver

    if ( strcmp(method, "MPI") == 0 ) {
      method_mpi->gather_names("", do_split);
      mpi_initialized = true;
    }
    else if ( strcmp(method, "TCP") == 0 ) {
      if ( has_port == 0 ) {
	mdi_error("Error in MDI_Init: -port option not provided");
      }
      if ( mpi_rank == 0 ) {
	MDI_Listen_TCP(port);
      }
    }
    else {
      mdi_error("Error in MDI_Init: method not recognized");
    }

  }
  else if ( strcmp(role,"ENGINE") == 0 ) {
    // initialize this code as an engine

    if ( strcmp(method, "MPI") == 0 ) {
      method_mpi->gather_names(name, do_split);
      mpi_initialized = true;
    }
    else if ( strcmp(method, "TCP") == 0 ) {
      if ( has_hostname == 0 ) {
	mdi_error("Error in MDI_Init: -hostname option not provided");
      }
      if ( has_port == 0 ) {
	mdi_error("Error in MDI_Init: -port option not provided");
      }
      if ( mpi_rank == 0 ) {
	MDI_Request_Connection_TCP(port, hostname);
      }
    }
    
  }
  else {
    mdi_error("Error in MDI_Init: role not recognized");
  }

  // set the MPI communicator correctly
  if ( mpi_initialized ) {
    if ( do_split ) {
      /*
      MPI_Comm* world_comm_ptr = (MPI_Comm*) world_comm;
      *world_comm_ptr = method_mpi->intra_MPI_comm;
      */
      method_mpi->split_mpi_communicator(world_comm);
    }
  }

  free( argv_line );

  return 0;
}


/*! \brief Accept a new MDI communicator
 *
 * The function returns an MDI_Comm that describes a connection between two codes.
 * If no new communicators are available, the function returns \p MDI_NULL_COMM.
 *
 */
MDI_Comm MDI_Accept_Communicator()
{
  int connection;

  // if MDI hasn't returned some connections, do that now
  //if ( returned_comms < comms.size() ) {
  if ( returned_comms < communicators.size() ) {
    returned_comms++;
    return returned_comms;
  }

  // check for any production codes connecting via TCP
  if ( manager->tcp_socket > 0 ) {
    //accept a connection via TCP
    connection = accept(manager->tcp_socket, NULL, NULL);
    if (connection < 0) {
      perror("Could not accept connection");
      exit(-1);
    }

    Communicator* new_communicator = new CommunicatorTCP( MDI_TCP, connection );

    // if MDI hasn't returned some connections, do that now
    //if ( returned_comms < comms.size() ) {
    if ( returned_comms < communicators.size() ) {
      returned_comms++;
      return returned_comms;
    }
  }

  // unable to accept any connections
  return MDI_NULL_COMM;
}


/*! \brief Send data through the MDI connection
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       buf
 *                   Pointer to the data to be sent.
 * \param [in]       count
 *                   Number of values (integers, double precision floats, characters, etc.) to be sent.
 * \param [in]       datatype
 *                   MDI handle (MDI_INT, MDI_DOUBLE, MDI_CHAR, etc.) corresponding to the type of data to be sent.
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int MDI_Send(const char* buf, int count, MDI_Datatype datatype, MDI_Comm comm)
{
   if ( method_mpi->intra_rank != 0 ) {
     perror("Called MDI_Send with incorrect rank");
   }

   Communicator* send_comm = communicators[comm-1];
   send_comm->send(buf, count, datatype);

   return 0;
}


/*! \brief Receive data through the MDI connection
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       buf
 *                   Pointer to the buffer where the received data will be stored.
 * \param [in]       count
 *                   Number of values (integers, double precision floats, characters, etc.) to be received.
 * \param [in]       datatype
 *                   MDI handle (MDI_INT, MDI_DOUBLE, MDI_CHAR, etc.) corresponding to the type of data to be received.
 * \param [in]       comm
 *                   MDI communicator associated with the connection to the sending code.
 */
int MDI_Recv(char* buf, int count, MDI_Datatype datatype, MDI_Comm comm)
{
   if ( method_mpi->intra_rank != 0 ) {
     perror("Called MDI_Recv with incorrect rank");
   }

   Communicator* recv_comm = communicators[comm-1];
   recv_comm->recv(buf, count, datatype);

   return 0;
}


/*! \brief Send a command of length \p MDI_COMMAND_LENGTH through the MDI connection
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       buf
 *                   Pointer to the data to be sent.
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int MDI_Send_Command(const char* buf, MDI_Comm comm)
{
   if ( method_mpi->intra_rank != 0 ) {
     perror("Called MDI_Send_Command with incorrect rank");
   }
   int count = MDI_COMMAND_LENGTH;
   char command[MDI_COMMAND_LENGTH];

   strcpy(command, buf);
   return MDI_Send( &command[0], count, MDI_CHAR, comm );
}


/*! \brief Receive a command of length \p MDI_COMMAND_LENGTH through the MDI connection
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       buf
 *                   Pointer to the buffer where the received data will be stored.
 * \param [in]       comm
 *                   MDI communicator associated with the connection to the sending code.
 */
int MDI_Recv_Command(char* buf, MDI_Comm comm)
{
   if ( method_mpi->intra_rank != 0 ) {
     perror("Called MDI_Recv_Command with incorrect rank");
   }
   int count = MDI_COMMAND_LENGTH;
   int datatype = MDI_CHAR;

   return MDI_Recv( buf, count, datatype, comm );
}


/*! \brief Return a conversion factor between two units
 *
 * The function returns the conversion factor from \p in_unit to \p out_unit.
 *
 * \param [in]       in_unit
 *                   Name of the unit to convert from.
 * \param [in]       out_unit
 *                   Name of the unit to convert to.
 */
double MDI_Conversion_Factor(char* in_unit, char* out_unit)
{
  if ( strcmp(in_unit,"Angstrom") == 0 and strcmp(out_unit,"Bohr") == 0 ) {
    return MDI_ANGSTROM_TO_BOHR;
  }
  else {
    mdi_error("Unrecognized conversion requested in MDI_Conversion_Factor");
  }
}


int MDI_Get_MPI_Code_Rank()
{
  return method_mpi->mpi_code_rank;
}

void MDI_Set_MPI_Intra_Rank(int rank)
{
  method_mpi->intra_rank = rank;
}
