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

/* Spaces forward or backward 'records' records from the current position */

/* Note: 1/4" tape drives do NOT support any ioctl calls that back up a	*/
/* tape (MTBSF and MTBSR), so a negative records parameter is invalid.	*/
/* HOWEVER, this call does NOT return an error.  Also, MTFSR is not	*/
/* supported, nor does it return an error. So your tape thinks it is	*/
/* ok, when it really has no idea what postion it is in!		*/

int v2_space_record(struct tapestate *state, int records)
{
   int code;
   int index;
   struct mtop op;

   if (records==0)
      return SUCCESS;

   index = state->tindex;

   if (records > 0) {
      op.mt_op = MTFSR;
      op.mt_count = records;	/* Forward the appropriate number of records */
      code = ioctl(state->channel, MTIOCTOP, &op);

      /* In this case an unusual error has occurred, so the tape position */
      /* is probably lost.  Indicate this in the tape globals and return. */

      if (code == -1) {
         i_rec[index] = 0;
         i_file[index] = 0;
         return errno;
      }

      i_rec[index] += records;
   }
   else {
      op.mt_op = MTBSR;
      op.mt_count = (-records);	/* Make a negative argument positive */
      code = ioctl(state->channel, MTIOCTOP, &op);
      if (code == -1) {
         i_rec[index] = 0;
         i_file[index] = 0;
         return errno;
      }
      i_rec[index] += records;
   }

   return SUCCESS;
}

#else

int v2_space_record(struct tapestate * UNUSED(state), int UNUSED(records))
{
   return NO_TAPE_SUPPORT;
}

#endif

