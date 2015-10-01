#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <descrip.h>
#include <fibdef.h>
#include <ssdef.h>
#include <iodef.h>

/* Spaces forward or backward 'records' records from the current position */

int space_ansi_record(ansistate, records)
struct ansistate *ansistate;
int records;
{
   int code;
   int index;
   struct fibdef1 fib;
   struct dsc$descriptor fibdesc;

   if (records==0)
      return SUCCESS;

   fibdesc.dsc$w_length = sizeof(fib);
   fibdesc.dsc$a_pointer = &fib;

   memset(&fib, 0, sizeof(fib));
   fib.fib$w_cntrlfunc = FIB$C_SPACE;
/* Yes, I know the following conditional is illegal (it's not a feature	*/
/* define), but I don't know why there is a difference here... at least	*/
/* we're in VMS-specific code.						*/
#if ALPHA_ARCH
   fib.fib$l_cntrlval = records;
#else
   fib.fib$r_cntrlval_overlay.fib$l_cntrlval = records;
#endif

   code = sys$qiow(ansistate->io_event, ansistate->channel, IO$_ACPCONTROL,
		   &ansistate->iosb,0,0,
		   &fibdesc,0,0,0,0,0);
   if (code != SUCCESS)
      return code;

   code = ansistate->iosb.status;
   if (code != SUCCESS) {
      if (code==SS$_ENDOFFILE)
         return END_OF_FILE;
      else
         return code;
   }

   ansistate->position += records;		/* Update tape position */

   return SUCCESS;
}
