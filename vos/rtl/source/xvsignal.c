#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/* Take the status of a previous RTL subroutine call and print	*/
/* the appropriate error message.  Abort the program if		*/
/* requested to.						*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvsignal, XVSIGNAL) (
   int *unit,		/* In:unit # of file whose operation is being checked */
   int *status,		/* In: status being checked */
   int *abend_flag 	/* In: if TRUE, call abend() */
)

{
   zvsignal(*unit, *status, *abend_flag);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

void zvsignal(
   int unit,		/* In:unit # of file whose operation is being checked */
   int status,		/* In: status being checked */
   int abend_flag	/* In: if TRUE, call abend() */
)

{

   if (status == SUCCESS)
      return;				/* Just in case */

   v2_sys_msg(unit,status);

   if (abend_flag) {
      if (status <= 0 || status == SUCCESS)
	 v2_error_code = 0;			/* VICAR error */
      else
	 v2_error_code = status;		/* OS error */
      zabend();
   }

   return;
}
