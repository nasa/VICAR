#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include <errno.h>
#include <unistd.h>

/* Finish up a file by writing the EOF mark, etc. */

int v2_finish_file(int unit)
{
   int status;
   V2_OFFSET j;
   struct bufstate *bufstate;
   struct devstate *devstate;

   /* If file is an output file, update the eof in the file header */

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   devstate = &bufstate->devstate;

   status = SUCCESS;

   switch (devstate->device_type) {

#if RTL_USE_TAPE
      case DEV_TAPE:
         i_open[devstate->dev.tape.tindex] = FALSE;

         if (EQUAL(CURRENT_S_VALUE(OP), "WRITE")) {
            status = v2_double_eof(&devstate->dev.tape);
            if (status != SUCCESS)
               return status;
         }
         break;
#endif

      case DEV_ANSI:
         break;
      case DEV_MEMORY:
         break;

      case DEV_ARRAY:
      case DEV_DISK:

         if (!EQUAL(CURRENT_S_VALUE(OP),"READ")) {

/* Calculate the total number of bytes in the file depending on the type of */
/* file we have.  Unlabeled output files check the actual number of records */
/* written, while input files and labeled files use file size info.         */

            if ((CURRENT_I_VALUE(FLAGS) & NO_LABELS) &&
		(EQUAL(CURRENT_S_VALUE(OP),"WRITE"))) {
               j = bufstate->eof_record * bufstate->recsize;
            }
            else {
               j = CURRENT_I_VALUE(N1) * CURRENT_I_VALUE(PIX_SIZE) +
			CURRENT_I_VALUE(NBB);
               j =  j * ((CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3))
                    + CURRENT_I_VALUE(NLB))
                    + CURRENT_I_VALUE(LBLSIZE)
                    + CURRENT_I_VALUE(EOL_SIZE);
            }
#if FTRUNCATE_AVAIL_OS
            if (V2_FTRUNCATE(devstate->dev.disk.channel, j) != 0)
               status = errno;
            else
#endif
               status = SUCCESS;
         }
         break;

      default:
         break;
   }

   return status;
}
