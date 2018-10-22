/****************************************************************************/
/*                                                                          */
/* Stubs to enable MPI programs to run on one processor without the MPI     */
/* library being available.  The mpi_fake.h file needs to be included in    */
/* user code to make use of these methods.  They can, however, coexist      */
/* with the real MPI library... i.e. both this module and MPI can be in     */
/* the program's link statement together; one or the other will be used     */
/* depending on whether mpi.h or mpi_fake.h is included during compilation. */
/*									    */
/* DO NOT ATTEMPT to mix this with the real MPI library.  Use one or the    */
/* other exclusively.							    */
/*                                                                          */
/* Adapted for VICAR 2/3/2003 by Bob Deen                                   */
/*                                                                          */
/* Original comment block follows...                                        */
/****************************************************************************/
/*                                                                          */
/* 3D Nanoelectronic Modeling Program (NEMO3D)                              */
/*                                                                          */
/* (C) 2000 Jet Propulsion Laboratory                                       */
/*                                                                          */
/****************************************************************************/
/* 
   Adapted for NEMO 3D at the Jet Propulsion Laboratory
*/

/*
 *  
 *  ********************************************************************* 
 *  (C) COPYRIGHT 1995 UNIVERSITY OF CHICAGO 
 *  *********************************************************************
 *  
 *  This software was authored by
 *  
 *  D. Levine
 *  Mathematics and Computer Science Division Argonne National Laboratory
 *  Argonne IL 60439
 *  levine@mcs.anl.gov
 *  (708) 252-6735
 *  (708) 252-5986 (FAX)
 *  
 *  with programming assistance of participants in Argonne National 
 *  Laboratory's SERS program.
 *  
 *  This program contains material protectable under copyright laws of the 
 *  United States.  Permission is hereby granted to use it, reproduce it, 
 *  to translate it into another language, and to redistribute it to 
 *  others at no charge except a fee for transferring a copy, provided 
 *  that you conspicuously and appropriately publish on each copy the 
 *  University of Chicago's copyright notice, and the disclaimer of 
 *  warranty and Government license included below.  Further, permission 
 *  is hereby granted, subject to the same provisions, to modify a copy or 
 *  copies or any portion of it, and to distribute to others at no charge 
 *  materials containing or derived from the material.
 *  
 *  The developers of the software ask that you acknowledge its use in any 
 *  document referencing work based on the  program, such as published 
 *  research.  Also, they ask that you supply to Argonne National 
 *  Laboratory a copy of any published research referencing work based on 
 *  the software.
 *  
 *  Any entity desiring permission for further use must contact:
 *  
 *  J. Gleeson
 *  Industrial Technology Development Center Argonne National Laboratory
 *  Argonne IL 60439
 *  gleesonj@smtplink.eid.anl.gov
 *  (708) 252-6055
 *  
 *  ******************************************************************** 
 *  DISCLAIMER
 *  
 *  THIS PROGRAM WAS PREPARED AS AN ACCOUNT OF WORK SPONSORED BY AN AGENCY 
 *  OF THE UNITED STATES GOVERNMENT.  NEITHER THE UNIVERSITY OF CHICAGO, 
 *  THE UNITED STATES GOVERNMENT NOR ANY OF THEIR EMPLOYEES MAKE ANY 
 *  WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR 
 *  RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY 
 *  INFORMATION OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT 
 *  INFRINGE PRIVATELY OWNED RIGHTS.
 *  
 *  ********************************************************************** 
 *  GOVERNMENT LICENSE
 *  
 *  The Government is granted for itself and others acting on its behalf a 
 *  paid-up, non-exclusive, irrevocable worldwide license in this computer 
 *  software to reproduce, prepare derivative works, and perform publicly 
 *  and display publicly.
 */

/******************************************************************************
*     FILE: mpi_stub.c:  MPI stubs needed for PGAPack operation without
*                        linking with a real MPI.
*
*     Authors: Brian P. Walenz
******************************************************************************/


#include "mpi_fake.h"


/*  Places the address of "location" into "address"
 *  In FORTRAN, does not return anything.
 */
int MPIfake_Address(void *location, MPI_Aint *address) {
    *address = (MPI_Aint)NULL;
    return(0);
}


/*  Broadcast "buf" to all processes.
 *  FORTRAN adds integer ierror to the end of the parameters.
 */
int MPIfake_Bcast(void *buf, int count, MPI_Datatype datatype, int root, MPI_Comm comm) {
    return(0);
}

/*  Barrier for all processes.  Do nothing for the fake version.
 */
int MPIfake_Barrier(MPI_Comm comm) {
    return(0);
}


/*  Duplicates communicator "comm" into "newcomm"
 *  FORTRAN has a third parameter, integer ie, and does not return anything.
 */
int MPIfake_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm) {
    return(0);
}


/*  Frees a communicator.   */
int MPIfake_Comm_free(MPI_Comm *comm) {
    return(0);
}


/*  Returns the rank of the current process in rank.  We return
 *  0 -- we are the master.
 */
int MPIfake_Comm_rank(MPI_Comm comm, int *rank) {
    *rank = 0;
    return(0);
}


/*  Returns the number of processors that are in communicator comm
 *  in size.  Always 1.
 */
int MPIfake_Comm_size(MPI_Comm comm, int *size) {
    *size = 1;
    return(0);
}

/*  Finalizes MPI.  */
int MPIfake_Finalize(void) {
    return(0);
}


/*  Initializes MPI.
 *  Ideally, we should parse the command-line and remove MPI arguments.
 */
int MPIfake_Init(int *argc, char ***argv) {
    return(0);
}


/*  Returns 1 in flag if MPI is already running.  It is.  */
int MPIfake_Initialized(int *flag) {
    *flag = 1;
    return(0);
}


/*  Waits for messages to us with tag "tag".  Sets status->MPI_SOURCE to the
 *  source of the message, status->MPI_TAG to the tag, and status->MPI_ERROR
 *  to 0.
 */
int MPIfake_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status) {
    status->MPI_SOURCE = source;
    status->MPI_TAG    = tag;
    status->MPI_ERROR  = 0;
    return(0);
}


/*  Send a message to a process.  */
int MPIfake_Send(void* buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm) {
    return(0);
}


/*  Receive a message from a source. */
int MPIfake_Recv(void* buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status) {
    status->MPI_SOURCE = source;
    status->MPI_TAG    = tag;
    status->MPI_ERROR  = 0;
    return(0);
}


int MPIfake_Sendrecv(void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 int dest, int sendtag, void *recvbuf, int recvcount,
                  MPI_Datatype recvtype, int source, int recvtag,
                  MPI_Comm comm, MPI_Status *status) {
    status->MPI_SOURCE = source;
    status->MPI_TAG    = recvtag;
    status->MPI_ERROR  = 0;
    return(0);
}


int MPIfake_Type_commit(MPI_Datatype *datatype) {
    return(0);
}


int MPIfake_Type_free(MPI_Datatype *datatype) {
    return(0);
}


int MPIfake_Type_struct(int count, int *array_of_blocklengths,
                    MPI_Aint *array_of_displacements,
                    MPI_Datatype *array_of_types, MPI_Datatype *newtype) {
    return(0);
}


int MPIfake_Get_count(MPI_Status *status, MPI_Datatype datatype, int *count){
   return 0;
}

