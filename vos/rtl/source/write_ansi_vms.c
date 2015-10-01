#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <iodef.h>

/* Write a number of blocks out to an ANSI-labelled tape */

int v2_write_ansi(ansistate, buf, block, nblocks, async_flag, transfer_count)
struct ansistate *ansistate;
char *buf;
V2_OFFSET block, nblocks;
int async_flag;
int *transfer_count;
{
   int delta;
   int code;
   int i;

/* Note:  ansistate->phys_blocksize is >= ansistate->blocksize.  So, we	*/
/* write data using phys_blocksize to fill the wasted space, but only	*/
/* advance the buffer by blocksize.  Thus, some data is potentially 	*/
/* written twice:  once into the waste area, and once at the beginning	*/
/* of the next block.  This should not cause a problem.			*/

   *transfer_count = 0;

   delta = block - ansistate->position;

   if (delta != 0) {
      if (space_ansi_record(ansistate, delta) != SUCCESS) /* updates position */
         return TAPE_POSITIONING_ERROR;
   }

   for (i=0; i<nblocks; i++) {

      if (i == nblocks-1)			/* only async on last one */
	 code = sys$qio (ansistate->io_event,
			 ansistate->channel,
			 IO$_WRITEVBLK,
			 &ansistate->iosb,
			 0,0,
			 buf,
			 ansistate->phys_blocksize,
			 0,0,0,0);
      else
	 code = sys$qiow(ansistate->io_event,
			 ansistate->channel,
			 IO$_WRITEVBLK,
			 &ansistate->iosb,
			 0,0,
			 buf,
			 ansistate->phys_blocksize,
			 0,0,0,0);

      if (code != SUCCESS)
	 return code;

      buf += ansistate->blocksize;
      ansistate->position++;

      if (i != nblocks-1) {
	 *transfer_count += MIN(ansistate->iosb.transfer_count,
				ansistate->blocksize);
	 code = ansistate->iosb.status;

	 if (code != SUCCESS)
	    return code;
      }
   }

   return SUCCESS;

}
