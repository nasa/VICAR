/******************************************************************************/
/*                                                                            */
/* Program: zvzinit.c                                                         */
/*                                                                            */
/* Purpose: This used to be the main program which all VICAR applications     */
/* written in C use to "start-up".  Now, that main program simply calls this  */
/* routine.                                                                   */
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
#define I_XIINC		/* Prevent XIINC include.  #undef if it's needed */
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
#include "rtlintproto.h"
#endif

#if VMS_OS		/* For compatibility with old applications only! */
#include "v2$inc:errdefs.h"
#else
#include "errdefs.h"
#endif

PUBLICREF int in_vicar;

#ifdef SYNTAX_CHECK
#undef SYNTAX_CHECK
#endif
#define SYNTAX_CHECK 256
#define NO_MESSAGE 512

int zvzinit(int argc, char *argv[])
{
#if RTL_USE_TAE
  struct LARGE_PARBLK *parb=NULL;
   void *file=NULL;
   int lines, cols, type;
   int n, swtch;
   char pname[40], msg[80];

   zvpblk((void **)&parb);
   t_pinit(&lines, &cols, &type);	/* avoid screen clear on startup */
   file = z_init((struct LARGE_PARBLK*) parb, P_CONT);
#endif
#if RTL_USE_SHELL_VIC
   if (file==NULL) {
      in_vicar=0;		/* not in the TAE - using shell */
      if (zzinit((struct PARBLK*) parb, argc, argv) != 0)
         file = (void *) &file;	/* Any nonnull value is fine */
   }
#endif

   zv_rtl_init();			/* Initialize the run-time library */

#if RTL_USE_TAE
   zvp("$switch", &swtch, &n);		/*  syntax checking requested? */
   if (((swtch & SYNTAX_CHECK) == 0) && (file != NULL)) {
      zvpinit(parb);			/* Load file parms if present */
      if ((swtch & NO_MESSAGE) == 0) {
         zvp( "_PROC", pname, &n);	/* get program name */
         sprintf(msg, "Beginning VICAR task %s", pname);
         zvmessage(msg, "");
      }
      return 1;			/* TRUE */
   }
   return 0;			/* FALSE */
#else
   return 1;			/* TRUE */
#endif

}
