#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <sys/file.h>
#include <errno.h>

/* Write a number of blocks out to a foreign tape */

int v2_write_tape(struct tapestate *state, char *buf,
	V2_OFFSET block, V2_OFFSET nblocks, int async_flag, int *transfer_count)
{    
   int bytes = 0, code, i;
   int blocksize;
   int index;
   int delta;

   index = state->tindex;
   delta = block+1 - i_rec[index];	/* i_rec is 1 based, block is 0 based */

   if (delta != 0) {
      if (v2_space_record(state, delta) != SUCCESS)  /* updates i_rec[index] */
         return TAPE_POSITIONING_ERROR;
   }

   for (i=0; i<nblocks; i++) {
      blocksize = state->blocksize;
      if ((blocksize & 1) == 1)	/* Odd size tape block, so make it even */
         blocksize++;		/* since the xt driver can't do odd blocks */
      code = write(state->channel, buf, blocksize);
      if (code == -1)
         return errno;
      if ((state->blocksize & 1) == 1 && code != 0)
         code--;			/* compensate for odd blocks */
      if (code != state->blocksize)
         return END_OF_VOLUME;		/* what else could cause this? */
      bytes += code;
      buf += code;
   }

   state->pos += bytes;
   *transfer_count = bytes;
   i_rec[index] += nblocks;

   return SUCCESS;
}

#else

int v2_write_tape(struct tapestate * UNUSED(state), char * UNUSED(buf),
	V2_OFFSET UNUSED(block), V2_OFFSET UNUSED(nblocks),
	int UNUSED(async_flag), int * UNUSED(transfer_count))
{
   return NO_TAPE_SUPPORT;
}

#endif

