#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <iodef.h>
#include <ssdef.h>

/* Read in some blocks from an ANSI-labelled tape */

int v2_read_ansi(ansistate, buf, block, nblocks, async_flag, transfer_count)
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
/* read data using phys_blocksize to fill the wasted space and to make	*/
/* sure the tape position is correct, but only advance the buffer by	*/
/* blocksize.  Thus, some data is read past the end of the buffer.	*/
/* This must be compensated for in the caller by making the buffer	*/
/* bufstate->buf_extra larger than needed!				*/

   if (ansistate->io_event == 0)
      async_flag = FALSE;

   *transfer_count = 0;

   delta = block - ansistate->position;

   if (delta != 0) {
      if (space_ansi_record(ansistate, delta) != SUCCESS) /* adjusts position */
         return TAPE_POSITIONING_ERROR;
   }

   for (i=0; i<nblocks; i++) {

      if (async_flag && i == nblocks-1)		/* only async on last one */
	 code = sys$qio (ansistate->io_event,
			 ansistate->channel,
			 IO$_READVBLK,
			 &ansistate->iosb,
			 0,0,
			 buf,
			 ansistate->phys_blocksize,
			 0,0,0,0);
      else
	 code = sys$qiow(ansistate->io_event,
			 ansistate->channel,
			 IO$_READVBLK,
			 &ansistate->iosb,
			 0,0,
			 buf,
			 ansistate->phys_blocksize,
			 0,0,0,0);

      if (code != SUCCESS)
	 return code;

      buf += ansistate->blocksize;
      ansistate->position++;

      if (!async_flag || i != nblocks-1) {
	 *transfer_count += MIN(ansistate->iosb.transfer_count,
				ansistate->blocksize);
	 code = ansistate->iosb.status;

	 if (code == SS$_ENDOFFILE) {	/* EOF on ANSI doesn't move tape */
	    ansistate->position--;	/* (if read again, get EOF again) */
	    if (*transfer_count == 0)
	       return END_OF_FILE;	/* EOF only if we read nothing */
	    else
	       return SUCCESS;
	 }

	 if (code != SUCCESS && code != SS$_DATAOVERUN)
	    return code;
      }
   }

   return SUCCESS;

}
