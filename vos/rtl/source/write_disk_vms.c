#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <iodef.h>

/* Write a number of blocks to the disk */

int v2_write_disk(state, buf, block, nblocks, async_flag, transfer_count)
struct diskstate *state;
char *buf;
V2_OFFSET block, nblocks;
int async_flag;
int *transfer_count;
{
   V2_OFFSET len;
   int code;

   if (block + nblocks > state->allocation) {
      code = v2_extend_disk_file(state, block + nblocks - state->allocation);
      if (code != SUCCESS)
	 return code;
   }

   len = nblocks * state->blocksize;

   code = sys$qio (state->io_event,
		   state->channel,
		   IO$_WRITEVBLK,
		   &state->iosb,
		   0,0,
		   buf,
		   (int)len,
		   (int)block+1,
		   0,0,0);

   if (code != SUCCESS)
      return code;

   return SUCCESS;
}
