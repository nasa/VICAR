#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Check to see if the record after 'rec' is entirely in the buffer. */
/* If not, issue an async read request to put it in.		     */

int v2_read_cache(struct bufstate *bufstate, int rec)
{
   int nxtrec;
   V2_OFFSET location;
   int status;

   nxtrec = rec + 1;

   if (nxtrec < bufstate->first_complete_rec ||
      nxtrec > bufstate->last_complete_rec) {

      location = ((V2_OFFSET)nxtrec * bufstate->recsize) / bufstate->blocksize;

      if (bufstate->flags & BUF_ALIGN)
         location = (location / bufstate->bufsize) * bufstate->bufsize;

      status = v2_read_blocks(&bufstate->devstate, bufstate->buffer, location,
		 (V2_OFFSET)bufstate->nblocksinbuf, TRUE);	/* async read */
      if (status != SUCCESS)
         return status;

      v2_update_bufstate(bufstate, location);
      bufstate->flags |= BUF_VALID;
   }

   return SUCCESS;
}
