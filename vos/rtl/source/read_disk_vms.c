#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <iodef.h>
#include <ssdef.h>

/* Read some blocks in from a disk file */

int v2_read_disk(state, buf, block, nblocks, async_flag, transfer_count)
struct diskstate *state;
char *buf;
V2_OFFSET block, nblocks;
int async_flag;
int *transfer_count;
{
   V2_OFFSET len;
   int code;

   if (state->io_event == 0)
      async_flag = FALSE;

   len = nblocks * state->blocksize;

   if (async_flag)
      code = sys$qio (state->io_event,
		      state->channel,
		      IO$_READVBLK,
		      &state->iosb,
		      0,0,
		      buf,
		      (int)len,
		      (int)block+1,
		      0,0,0);
   else
      code = sys$qiow(state->io_event,
		      state->channel,
		      IO$_READVBLK,
		      &state->iosb,
		      0,0,
		      buf,
		      (int)len,
		      (int)block+1,
		      0,0,0);

   if (code != SUCCESS || async_flag)
      return code;

   *transfer_count = state->iosb.transfer_count;
   code = state->iosb.status;
   if (code == SS$_ENDOFFILE)
      if (*transfer_count > 0)
	 code = SUCCESS;	/* EOF only if we didn't read anything */
      else
	 code = END_OF_FILE;

   return code;
}
