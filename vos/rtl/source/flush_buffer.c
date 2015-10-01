#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* If there are unwritten blocks in the buffer, flush them out */

int v2_flush_buffer(struct bufstate *state)
{
   int status;

   if (state->flags & BUF_DIRTY) {
      status = v2_write_blocks(&state->devstate, state->buffer, state->blockno,
			(V2_OFFSET)state->nblocksinbuf, TRUE); /* async write */
      if (status != SUCCESS)
	 return status;

      state->flags &= ~BUF_DIRTY;
   }

   return SUCCESS;
}
