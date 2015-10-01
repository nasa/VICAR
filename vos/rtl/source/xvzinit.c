#include "xvmaininc.h"
#include "ftnbridge.h"
#if RTL_USE_TAE || RTL_USE_SHELL_VIC
#include "taeconf.inp"
#include "pgminc.inc"
#include "parblk.inc"
#endif
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

PUBLICREF int in_vicar;

#define S_DEBUG		1		/* Bits in $SWITCH */
#define S_SYNTAX_CHECK	256
#define S_NO_MESSAGE	512

#if RTL_USE_TAE
int FTN_NAME2(xzinit, XZINIT) (struct PARBLK*, int*, int*, int*, int*);
int FTN_NAME2(xzsinit, XZSINIT) (struct PARBLK*, int*, int*, int*, int*);
#endif

/************************************************************************/
/* This routine does most of the VICAR setup for a Fortran program.	*/
/* It is needed for a variety of reasons, such as eliminating the need	*/
/* for the %val function in xzinit, and to get around the lack of a	*/
/* consistently-named bitwise AND function.				*/
/*									*/
/* This function should *ONLY* be called from the Fortran application	*/
/* main include file.							*/
/*									*/
/* Replaces the fortran code:						*/
/* 	integer parb							*/
/*	call xvpblk(parb)						*/
/*	call xzinit(%val(parb), ..., status)				*/
/*	call xvp('$switch', switch, n)	! syntax checking requested?	*/
/*	if ((iand(switch,256).eq.0) .and. (status.eq.xsucc)) then	*/
/*	   call xvpinit(%val(parb))					*/
/*	   if (iand(switch,512).eq.0) then	! NO_MESS not on	*/
/*	      call xvp('_PROC', pname, n)				*/
/*	      call xvmessage('Beginning VICAR task '//pname, ' ')	*/
/*	   endif							*/
/*	   !!!call main44!!!  done by include file			*/
/************************************************************************/

void FTN_NAME2(xvzinit, XVZINIT) (int *lun, int *flag, int *debug)
{
   int status;
#if RTL_USE_TAE
   struct LARGE_PARBLK *parb;
   int parblksize = sizeof(struct LARGE_PARBLK) / sizeof(int); /* size in ints*/
   int lines, cols, type;			/* for t_pinit() */
   int mode, swtch, n;
   char pname[40];
   char msg[80];
#endif

   *flag = FALSE;

   status = !SUCCESS;
#if RTL_USE_TAE
   zvpblk((void **)&parb);
   t_pinit(&lines, &cols, &type);	/* avoid screen clear on startup */
   mode = P_CONT;
   FTN_NAME2(xzinit, XZINIT)((struct PARBLK*) parb, &parblksize, lun, 
			    &mode, &status);
#endif
#if RTL_USE_SHELL_VIC
   if (status != SUCCESS) {
     in_vicar = 0;
     FTN_NAME2(xzsinit,XZSINIT)((struct PARBLK*)parb, &parblksize, lun, 
			      &mode, &status);
   }
#endif

   zv_rtl_init();			/* Initialize the run-time library */

   applic_lang = FORTRAN_LANG;

#if RTL_USE_TAE
   zvp("$switch", &swtch, &n);		/* syntax checking requested? */

   *debug = (swtch & S_DEBUG) != 0;
   if (((swtch & S_SYNTAX_CHECK) == 0) && (status == SUCCESS)) {
      zvpinit(parb);			/* Load file parms if present */
      if ((swtch & S_NO_MESSAGE) == 0) {
         zvp( "_PROC", pname, &n);	/* get program name */
         sprintf(msg, "Beginning VICAR task %s", pname);
         zvmessage(msg, "");
      }
      *flag = TRUE;
   }
#else
   *flag = TRUE;
   *debug = FALSE;
#endif

   return;
}
