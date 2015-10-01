#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <iodef.h>
#include <ssdef.h>

/* Spaces forward or backward 'records' records from the current position */

int space_record(state, records)
struct tapestate *state;
int records;
{
   int code;
   int index;

   if (records==0)
      return SUCCESS;

   code = sys$qiow(state->io_event, state->channel, IO$_SKIPRECORD,
		   &state->iosb,0,0,records,0,0,
		   0,0,0);
   if (code != SUCCESS)
      return code;

   index = state->tindex;

/* In this case an unusual error has occurred, so the tape position	*/
/* is probably lost.  Indicate this in the tape globals and return.	*/

   if (state->iosb.status != SUCCESS && state->iosb.status != SS$_ENDOFFILE) {
      i_rec[index] = 0;
      i_file[index] = 0;
      code=state->iosb.status;
      return code;
   }

   if (records>0)			/* Update TAE globals and return */
      i_rec[index] += state->iosb.transfer_count;
   else
      i_rec[index] -= state->iosb.transfer_count;

   if (state->iosb.status==SS$_ENDOFFILE)
      return END_OF_FILE;

   return SUCCESS;
}

#else

int space_record()
{
   return NO_TAPE_SUPPORT;
}

#endif

