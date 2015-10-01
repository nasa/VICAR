#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Write a number of blocks out to the file */

int v2_write_blocks(
   struct devstate *devstate,
   char *buf,
   V2_OFFSET block,
   V2_OFFSET nblocks,
   int async_flag	/* included for orthogonality... not used under Unix */
)

{
   int status;

   switch (devstate->device_type) {

      case DEV_DISK:
         status = v2_write_disk(&devstate->dev.disk, buf, block, nblocks,
		  async_flag, &devstate->transfer_count, devstate->file_offset);
         break;

#if RTL_USE_TAPE
      case DEV_TAPE:
         status = v2_write_tape(&devstate->dev.tape, buf, block, nblocks,
			     async_flag, &devstate->transfer_count);
         break;
#endif

      case DEV_ARRAY:
         status = v2_write_nop();
         break;

      default:
         status = NOT_IMPLEMENTED;
   }

   return status;
}
