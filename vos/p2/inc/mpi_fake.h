/****************************************************************************/
/*									    */
/* Stubs to enable MPI programs to run on one processor without the MPI     */
/* library being available.  Either the real mpi.h, or this file, should    */
/* be included in user code... NOT BOTH.				    */
/*									    */
/* Adapted for VICAR 2/3/2003 by Bob Deen				    */
/*									    */
/* Original comment block follows...					    */
/****************************************************************************/
/*                                                                          */
/* 3D Nanoelectronic Modeling Program (NEMO3D)                              */
/*                                                                          */
/* (C) 2000 Jet Propulsion Laboratory                                       */
/*                                                                          */
/****************************************************************************/
/*                                                                          */
/* Adopted to work with NEMO 3D 
   Author:  Gerhard Klimeck
*/

/*  These are the definitions to use our fake version of MPI in mpi_stub.c 
 *  Guaranteed only to make PGAPack compile and link without MPI, but should
 *  work from user C and fortran programs.
 *
 *  Some of these are from mpich's mpi.h, others are custom.
 *
 *  Author: Brian P. Walenz
 */

#ifndef MPI_FAKE_H
#define MPI_FAKE_H 1


#include <stdio.h>

#include "xvmaininc.h"			/* just for __cplusplus */
#ifdef __cplusplus
extern "C" {
#endif

typedef void *  MPI_Comm;
typedef void *  MPI_Datatype;
typedef long    MPI_Aint;
typedef int     MPI_Op;

typedef struct {
    int     MPI_SOURCE;
    int     MPI_TAG;
    int     MPI_ERROR;
} MPI_Status;

#define MPI_BYTE            (void *)NULL
#define MPI_CHAR            (void *)NULL
#define MPI_DOUBLE          (void *)NULL
#define MPI_FLOAT           (void *)NULL
#define MPI_INT             (void *)NULL
#define MPI_LONG            (void *)NULL
#define MPI_LONG_DOUBLE     (void *)NULL
#define MPI_PACKED          (void *)NULL
#define MPI_SHORT           (void *)NULL
#define MPI_UNSIGNED_CHAR   (void *)NULL
#define MPI_UNSIGNED        (void *)NULL
#define MPI_UNSIGNED_LONG   (void *)NULL
#define MPI_UNSIGNED_SHORT  (void *)NULL

#define MPI_COMM_WORLD      (void *)NULL
#define MPI_COMM_SELF       (void *)NULL

#define MPI_BOTTOM          (void *)0

#define MPI_PROC_NULL       (-1)
#define MPI_ANY_SOURCE      (-2)
#define MPI_ANY_TAG         (-1)
#define MPI_SUM             (MPI_Op)(102)

/*  Declare prototypes for the MPI functions.  */

#define MPI_Address MPIfake_Address
int MPIfake_Address(void *, MPI_Aint *);

#define MPI_Bcast MPIfake_Bcast
int MPIfake_Bcast(void *, int, MPI_Datatype, int, MPI_Comm);

#define MPI_Barrier MPIfake_Barrier
int MPIfake_Barrier(MPI_Comm);

#define MPI_Comm_dup MPIfake_Comm_dup
int MPIfake_Comm_dup(MPI_Comm, MPI_Comm *);

#define MPI_Comm_free MPIfake_Comm_free
int MPIfake_Comm_free(MPI_Comm *);

#define MPI_Comm_rank MPIfake_Comm_rank
int MPIfake_Comm_rank(MPI_Comm, int *);

#define MPI_Comm_size MPIfake_Comm_size
int MPIfake_Comm_size(MPI_Comm, int *);

#define MPI_Finalize MPIfake_Finalize
int MPIfake_Finalize(void);

#define MPI_Init MPIfake_Init
int MPIfake_Init(int *, char ***);

#define MPI_Initialized MPIfake_Initialized
int MPIfake_Initialized(int *);

#define MPI_Probe MPIfake_Probe
int MPIfake_Probe(int, int, MPI_Comm, MPI_Status *);

#define MPI_Send MPIfake_Send
int MPIfake_Send(void *, int, MPI_Datatype, int, int, MPI_Comm);

#define MPI_Recv MPIfake_Recv
int MPIfake_Recv(void *, int, MPI_Datatype, int, int, MPI_Comm, MPI_Status *);

#define MPI_Sendrecv MPIfake_Sendrecv
int MPIfake_Sendrecv(void *, int, MPI_Datatype, int, int, void *, int,
		     MPI_Datatype, int, int, MPI_Comm, MPI_Status *);

#define MPI_Type_commit MPIfake_Type_commit
int MPIfake_Type_commit(MPI_Datatype *);

#define MPI_Type_free MPIfake_Type_free
int MPIfake_Type_free(MPI_Datatype *);

#define MPI_Type_struct MPIfake_Type_struct
int MPIfake_Type_struct(int, int *, MPI_Aint *, MPI_Datatype *, MPI_Datatype *);

#define MPI_Get_count MPIfake_Get_count
int MPIfake_Get_count(MPI_Status *, MPI_Datatype, int *);

#ifdef __cplusplus
}
#endif

#endif /* MPI_FAKE_H */

