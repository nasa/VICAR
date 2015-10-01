#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Update the state variables to reflect a buffer starting at */
/* block number 'location'.				      */

void v2_update_bufstate(struct bufstate *bufstate, V2_OFFSET location)
{
   V2_OFFSET byteloc, endloc;
   int recsize, recsperbuf;

   recsize = bufstate->recsize;

   byteloc = location * bufstate->blocksize;

   bufstate->blockno = location;

/* This is for the benefit of old tapes, which may have dead space at the end */
/* of a block.								      */

   if (bufstate->flags & BUF_ALIGN) {
      recsperbuf = bufstate->bufsize / recsize;
      bufstate->first_rec = (location / bufstate->nblocksinbuf) * recsperbuf;
      bufstate->last_rec = bufstate->first_rec + recsperbuf - 1;
      bufstate->first_complete_rec = bufstate->first_rec;
      bufstate->last_complete_rec = bufstate->last_rec;

      bufstate->last_rec_len = recsize;
      bufstate->last_rec_pos = (recsperbuf-1) * recsize;
      bufstate->first_rec_pos = 0;
      bufstate->rec_offset = 0;
   }
   else {				/* This is for normal buffers */
      bufstate->first_rec = byteloc / recsize;
      bufstate->first_rec_pos = byteloc % recsize;
      bufstate->rec_offset = recsize - bufstate->first_rec_pos;
      if (bufstate->first_rec_pos == 0)
	 bufstate->rec_offset = 0;   /* record starts exactly at buffer start */

      endloc = byteloc + bufstate->bufsize-1; /* file pos of last byte in buf */

      bufstate->last_rec = endloc / recsize;
      bufstate->last_rec_len = (endloc % recsize) + 1;
      bufstate->last_rec_pos = bufstate->bufsize - bufstate->last_rec_len;

      bufstate->first_complete_rec = (byteloc+recsize-1) / recsize;
      bufstate->last_complete_rec = (endloc-recsize+1) / recsize;
   }
}

