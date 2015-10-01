#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

/* If a tape is unlabeled its block size can not be determined from the	*/
/* label; this routine determines the block size and updates it in the	*/
/* current unit table.  'Read' if set to READ_OK, indicates that this	*/
/* routine may attempt to read the tape to determine the block size. If	*/
/* set to DONT_READ a tape read is prohibited.				*/

/* Temporary kludge. Need to fix.  38908 is the largest number of bytes	*/
/* the tape drive has been able to read at one time.  It has been set	*/
/* larger for safety.							*/

#define HUGE_BLOCKSIZE 65534
#define DEFAULT_BLOCKSIZE 8192		/* arbitrary */

int v2_det_tape_blksize(int unit, int read_ok, int index, int file)
{
   struct bufstate  *bufstate;
   struct tapestate *tapestate;
   int status, blocksize;
   char *buffer;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   tapestate = &bufstate->devstate.dev.tape;

   /* We need a way for the user to specify the blocksize.  Until then,	*/
   /* don't return anything for output tapes.  The blocksize will get	*/
   /* set by v2_initialize_buffer().  Note that the mt_bf field in the	*/
   /* MTIOCGET ioctl call is apparently useless.			*/

   blocksize = 0;

   if (read_ok == DONT_READ) {
      if (blocksize != 0) {
         bufstate->blocksize = blocksize;
         tapestate->blocksize = blocksize;
      }
      return SUCCESS;
   }

   /* If we can read from the file, see if it is at eof. If it is not,	*/
   /* try and find out the accurate block size by seeing what a REALLY	*/
   /* big read returns.							*/

   buffer = (char *)malloc(HUGE_BLOCKSIZE);
   if (buffer == NULL)
      return INSUFFICIENT_MEMORY;
   tapestate->blocksize = HUGE_BLOCKSIZE;

   status = v2_read_blocks(&bufstate->devstate, buffer,
			(V2_OFFSET)0, (V2_OFFSET)1,
			FALSE);
   free(buffer);
   if (status != SUCCESS)
      return status;

   if (bufstate->devstate.transfer_count != 0) {
      tapestate->blocksize = bufstate->devstate.transfer_count;
      bufstate->blocksize = bufstate->devstate.transfer_count;
      CURRENT_I_VALUE(BUFSIZE) = bufstate->devstate.transfer_count;
   }
   else {		/* read 0, so something's wrong... use the default */
      tapestate->blocksize = DEFAULT_BLOCKSIZE;
      bufstate->blocksize = DEFAULT_BLOCKSIZE;
      CURRENT_I_VALUE(BUFSIZE) = DEFAULT_BLOCKSIZE;
   }

   return SUCCESS;
}

#else

int v2_det_tape_blksize(int UNUSED(unit), int UNUSED(read_ok),
			int UNUSED(index), int UNUSED(file))
{
   return NO_TAPE_SUPPORT;
}

#endif

