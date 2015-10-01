#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <dvidef.h>
#include <ssdef.h>

/* If a tape is unlabeled its block size can not be determined from the	     */
/* label; this routine determines the block size and updates it in the	     */
/* current unit table.  'Read' if set to READ_OK, indicates that this	     */
/* routine may attempt to read the tape to determine the block size. If set  */
/* to DONT_READ a tape read is prohibited.				     */

int v2_det_tape_blksize(unit,read)
int unit;
int read;
{
   struct bufstate *bufstate;
   struct tapestate *tapestate;
   int status;
   char *temp;
   int size;
   int rec;
   short dummy;		/* used for the return len from getdvi */

struct {
   short length;
   short code;
   int *addr;
   short *ret_len;
   int zero;
} item = {4,DVI$_DEVBUFSIZ,&rec,&dummy,0};

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   tapestate = &bufstate->devstate.dev.tape;

   status = sys$getdviw(0,tapestate->channel,0,&item,0,0,0,0);
   if (status == SS$_ENDOFFILE)
      return END_OF_FILE;
   if (status != SUCCESS)
      return status;

/* If the mount command for this tape specified the blocksize then use it,  */
/* else determine the block size by reading the tape.  Try a read that is   */
/* larger than a block could possibly be... only one block's worth will     */
/* actually be read.							    */

   if (rec != 0) { 
      bufstate->blocksize = rec;
      tapestate->blocksize = rec;
      CURRENT_I_VALUE(BUFSIZE) = rec;
      return SUCCESS;
   }

   if (read == DONT_READ)
      return SUCCESS;

   size = 65535;
   temp = malloc(size);
   if (temp == NULL)
      return INSUFFICIENT_MEMORY;
   tapestate->blocksize = size;

   status = v2_read_blocks(&bufstate->devstate, temp,
			(V2_OFFSET)0, (V2_OFFSET)1,
			FALSE);
   free(temp);
   if (status != SUCCESS && status != SS$_DATAOVERUN)
      return status;

   tapestate->blocksize = bufstate->devstate.transfer_count;
   bufstate->blocksize = bufstate->devstate.transfer_count;
   CURRENT_I_VALUE(BUFSIZE) = bufstate->devstate.transfer_count;

   return SUCCESS;
}

#else

int v2_det_tape_blksize()
{
   return NO_TAPE_SUPPORT;
}

#endif

