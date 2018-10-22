/*    suspend execution for HUNDREDTHS x 0.01 (wall clock) seconds.  */
#include <sys/param.h>
#include <sys/types.h>
#include <sys/time.h>

#include "xvmaininc.h"
#include "vmachdep.h"
#include "ftnbridge.h"

#ifndef NULL
#define NULL ((void *)0)
#endif

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/
void zvwait(int hundredths );


void FTN_NAME2(vwait, VWAIT) ( hundredths )
  int *hundredths;	
{
   zvwait( *hundredths );
}

/************************************************************************/
/* C-Callable Version       The method using select was proposed by Alan Mazer
            as the most universal under UNIX.  usleep is an alternative where
            available, but neither usleep or select appear to be in ANSI C.*/
/************************************************************************/

void zvwait(int hundredths )
{
#if SELECT_AVAIL_OS
    struct timeval timeout;

    timeout.tv_sec  = hundredths/100;   /*  run a timer on a NULL pipe  */
    timeout.tv_usec = (hundredths*10000) % 1000000;
    (void)select(64,(fd_set*)NULL,(fd_set *)NULL,(fd_set *)NULL,
	&timeout); /*64 used instead of getdtablesize which is not a universal*/
#else
    sleep( (hundredths+50)/100 );     /*  perhaps this is good enough.
           				  Resolution of 1 second.  */
#endif
}
