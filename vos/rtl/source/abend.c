#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"
#include "rtlintproto.h"

/* Abnormally terminate an application */

/************************************************************************/
/* Fortran-Callable Version (Unix)					*/
/************************************************************************/

void FTN_NAME2(abend, ABEND) (void)
{
   zabend();
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

void zabend()
{
   static int called = FALSE;

   if (called)
      return;			/* Prevent recursive calling		*/
   called = TRUE;

   zvmessage(" ** ABEND called **", "");
   zvend(v2_error_code);	/* Not successful completion	*/

   return;
}
