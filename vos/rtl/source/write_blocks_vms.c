#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

/* Write a number of blocks out to the file */

v2_write_blocks(devstate, buf, block, nblocks, async_flag)
struct devstate *devstate;
char *buf;
V2_OFFSET block, nblocks;
int async_flag;		/* included for orthogonality... always assumed TRUE */
{
   int status;

   switch (devstate->device_type) {

      case DEV_DISK:
	 status = v2_write_disk(&devstate->dev.disk, buf, block, nblocks,
			     async_flag, &devstate->transfer_count);
	 if (async_flag)
	    devstate->async_pending = TRUE;
	 break;

#if RTL_USE_TAPE
      case DEV_TAPE:
	 status = v2_write_tape(&devstate->dev.tape, buf, block, nblocks,
			     async_flag, &devstate->transfer_count);
	 if (async_flag)
	    devstate->async_pending = TRUE;
	 break;
#endif

      case DEV_ANSI:
	 status = v2_write_ansi(&devstate->dev.ansi, buf, block, nblocks,
			     async_flag, &devstate->transfer_count);
	 if (async_flag)
	    devstate->async_pending = TRUE;
	 break;

      case DEV_MEMORY:
      case DEV_ARRAY:
	 status = write_nop();
	 break;

      default:
	 return NOT_IMPLEMENTED;
   }

   return status;
}
