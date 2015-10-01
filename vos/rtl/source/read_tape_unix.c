#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

/* Read some blocks from a tape file */

int v2_read_tape(struct tapestate *state, char *buf,
	V2_OFFSET block, V2_OFFSET nblocks, int async_flag, int *transfer_count)
{
   int index;
   int delta;
   int code;
   int blocksize;
   int i;
   struct mtop op;

   *transfer_count = 0;

   index = state->tindex;
   delta = block+1 - i_rec[index];	/* i_rec is 1 based, block is 0 based */

   if (delta != 0) {
      if (v2_space_record(state, delta) != SUCCESS)  /* adjusts i_rec[index] */
         return TAPE_POSITIONING_ERROR;
   }

   for (i=0; i<nblocks; i++) {

      errno = 0;
      blocksize = state->blocksize;
      if ((blocksize & 1) != 0)		/* odd-length reads don't work! */
         blocksize++;			/* so add one, which (amazingly) does */
      code = read(state->channel, buf, blocksize);
      if (code == -1)
         return errno;

      buf += state->blocksize;	/* Move pointer to next block in buffer */
      i_rec[index]++;

      *transfer_count += code;		/* *transfer_count is total # read */
      if (code < state->blocksize) {
         if (code == 0) {
            op.mt_op = MTBSF;		/* back up to read EOF again */
            op.mt_count = 1;
            code = ioctl(state->channel, MTIOCTOP, &op);
            if (code == -1)
               return errno;
         }
         if (*transfer_count == 0)
            return END_OF_FILE;		/* eof only if we read nothing */
         else
            return SUCCESS;
      }
   }

   return SUCCESS;

}

#else

int v2_read_tape(struct tapestate * UNUSED(state), char * UNUSED(buf),
	V2_OFFSET UNUSED(block), V2_OFFSET UNUSED(nblocks),
	int UNUSED(async_flag), int * UNUSED(transfer_count))
{
   return NO_TAPE_SUPPORT;
}

#endif

