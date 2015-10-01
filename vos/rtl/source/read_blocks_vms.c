#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

/* Read a number of blocks from the file */

v2_read_blocks(devstate, buf, block, nblocks, async_flag)
struct devstate *devstate;
char *buf;
V2_OFFSET block, nblocks;
int async_flag;
{
   int status;

/* Note:  The buffer must be declared bufstate->buf_extra larger than	*/
/* needed to handle idiosyncracies in ansi tapes (see v2_read_ansi or	*/
/* v2_write_ansi).  This also means that the area after the read data	*/
/* may be modified, so you cannot read a little bit into the beginning	*/
/* of a buffer and expect the rest of the buffer to remain unchanged.	*/

   switch (devstate->device_type) {

      case DEV_DISK:
	 status = v2_read_disk(&devstate->dev.disk, buf, block, nblocks,
			    async_flag, &devstate->transfer_count);
	 if (async_flag)
	    devstate->async_pending = TRUE;
	 break;

#if RTL_USE_TAPE
      case DEV_TAPE:
	 status = v2_read_tape(&devstate->dev.tape, buf, block, nblocks,
			    async_flag, &devstate->transfer_count);
	 if (async_flag)
	    devstate->async_pending = TRUE;
	 break;
#endif

      case DEV_ANSI:
	 status = v2_read_ansi(&devstate->dev.ansi, buf, block, nblocks,
			    async_flag, &devstate->transfer_count);
	 if (async_flag)
	    devstate->async_pending = TRUE;
	 break;

      case DEV_MEMORY:		/* same as array */
      case DEV_ARRAY:
	 status = read_nop();
	 break;

      default:
	 return NOT_IMPLEMENTED;
   }

   return status;
}
