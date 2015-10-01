#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* This routine returns the starting address for array I/O, after the labels */
/* have been taken into account.					     */

void *v2_array_io_location(int unit)
{
   struct bufstate *bufstate;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   if (bufstate->devstate.device_type == DEV_ARRAY)
      return bufstate->devstate.dev.array.start + CURRENT_I_VALUE(LBLSIZE);

   /* If not an array file, or mmap() not available, then return NULL */

   return NULL;

}
