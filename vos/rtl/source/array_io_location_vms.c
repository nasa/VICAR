#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

/* This routine returns the starting address for array I/O, after the labels */
/* have been taken into account.					     */

void *v2_array_io_location(unit)
int unit;
{
   struct bufstate *bufstate;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   if (bufstate->devstate.device_type == DEV_ARRAY)
     return bufstate->devstate.dev.array.start + CURRENT_I_VALUE(LBLSIZE);

   if (bufstate->devstate.device_type == DEV_MEMORY)
      return bufstate->devstate.dev.memory.start + CURRENT_I_VALUE(LBLSIZE);

   return NULL;		/* in case they try array i/o to a tape or something! */
}
