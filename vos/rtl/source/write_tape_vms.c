#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <iodef.h>

/* Write a number of blocks out to a foreign tape */

int v2_write_tape(state, buf, block, nblocks, async_flag, transfer_count)
struct tapestate *state;
char *buf;
V2_OFFSET block, nblocks;
int async_flag;
int *transfer_count;
{
   int index;
   int delta;
   int code;
   int i;

   *transfer_count = 0;

   index = state->tindex;
   delta = block+1 - i_rec[index];	/* i_rec is 1 based, block is 0 based */

   if (delta != 0) {
      if (space_record(state, delta) != SUCCESS)      /* updates i_rec[index] */
         return TAPE_POSITIONING_ERROR;
   }

   for (i=0; i<nblocks; i++) {

      if (i == nblocks-1)			/* only async on last one */
	 code = sys$qio (state->io_event,
			 state->channel,
			 IO$_WRITEVBLK,
			 &state->iosb,
			 0,0,
			 buf,
			 state->blocksize,
			 0,0,0,0);
      else
	 code = sys$qiow(state->io_event,
			 state->channel,
			 IO$_WRITEVBLK,
			 &state->iosb,
			 0,0,
			 buf,
			 state->blocksize,
			 0,0,0,0);

      if (code != SUCCESS)
	 return code;

      buf += state->blocksize;
      i_rec[index]++;

      if (i != nblocks-1) {
	 *transfer_count += state->iosb.transfer_count;
	 code = state->iosb.status;

	 if (code != SUCCESS)
	    return code;
      }
   }

   return SUCCESS;

}

#else

int v2_write_tape()
{
   return NO_TAPE_SUPPORT;
}

#endif

