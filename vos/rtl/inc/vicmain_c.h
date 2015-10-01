/******************************************************************************/
/*                                                                            */
/* Program: VICMAIN_C                                                         */
/*                                                                            */
/* Purpose: This is the main program which all VICAR applications written in  */
/*          C use to "start-up".                                              */
/*                                                                            */
/* Copyright (C) 1989, 1991 California Institute of Technology.               */
/* All rights reserved.  U.S. Government sponsorship under                    */
/* NASA contract NAS7-918 is acknowledged.                                    */
/*                                                                            */
/******************************************************************************/

#ifdef vms		/* For compatibility with old applications only! */
#include "v2$inc:xvmaininc.h"
#else
#include "xvmaininc.h"
#endif

#if RTL_USE_TAE
#ifndef I_XIINC
#define I_XIINC		/* Prevent XIINC include.  #undef if it's needed */
#endif
#if VMS_OS		/* For compatibility with old applications only! */
#include "tae$inc:pgminc.inc"
#else
#include "pgminc.inc"
#endif
#endif

/* sprintf() is a pain because some machines define it as returning	*/
/* int, and some return char *.  So, we can't just define the return	*/
/* type with no arguments, because the return type differs.  We have to	*/
/* include stdio.h.  For compatibility with apps that don't expect	*/
/* this, we only include stdio.h if prototypes are in use.		*/

#ifndef _NO_PROTO
#include <stdio.h>	/* for sprintf() definition */
#endif
#include "zvproto.h"

#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL (void*) 0
#endif
#endif

#if VMS_OS		/* For compatibility with old applications only! */
#include "v2$inc:errdefs.h"
#else
#include "errdefs.h"
#endif

#ifdef ENABLE_MPI
#include "mpi.h"
int mpi_numprocs, mpi_id;	/* GLOBAL variables for # nodes, node id */
#include <unistd.h>		/* for chdir() */
#include <stdlib.h>		/* for getenv() */
#endif

int main(int argc, char *argv[])
{

#ifdef ENABLE_MPI
/* Save the original argc for the directory hack... */
   int argc_check = argc;

/* MPI (Beowulf-style parallel processing) requires access to the command */
/* line, thus the messiness in here.  ENABLE_MPI should be defined by the */
/* vimake system via -D depending on whether LIB_MPI is present in the    */
/* imake file or not.                                                     */

   MPI_Init(&argc, &argv);
   MPI_Comm_size(MPI_COMM_WORLD, &mpi_numprocs);

   /* Horrid hack to work around a bug in the MPICH implementation.	*/
   /* When the program is run without using mpirun, MPI_Init does	*/
   /* a chdir() to where the executable is.  Not a good plan!!  So,	*/
   /* we check for that case and un-do the damage...			*/

   if (argc_check == argc && mpi_numprocs == 1) {
      char *env = NULL;
      env = getenv("PWD");
      if (env != NULL)
         chdir(env);
   }
#endif

   if (zvzinit(argc, argv)) {

#ifdef ENABLE_MPI
      MPI_Comm_rank(MPI_COMM_WORLD, &mpi_id);
      if (mpi_numprocs > 1) {
         char msg[256];
          sprintf(msg, "MPI CPU [%i]/%i active\n",mpi_id+1,mpi_numprocs);
          zvmessage(msg, "");
      }
#endif

      main44();

#ifdef ENABLE_MPI
      MPI_Finalize();           /* MPI_Finalize must be called before zvend! */
#endif

      zvend(1);			/* SUCCESS */
   }

#ifdef ENABLE_MPI
   else {
      MPI_Finalize();
   }
#endif
   return 1;
}

