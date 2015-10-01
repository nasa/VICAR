#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Close a file */

int v2_close_file(int unit)
{
   struct bufstate *bufstate;
   struct devstate *devstate;
   int status;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   devstate = &bufstate->devstate;

   status = v2_io_complete_check(devstate); /* wait for pending read/writes */
   if (status != SUCCESS)
      return status;

   status = v2_flush_buffer(bufstate);	/* flush the buffer */
   if (status != SUCCESS)
      return status;

   status = v2_io_complete_check(devstate);	/* wait for the flush */
   if (status != SUCCESS)
      return status;

   status = v2_finish_file(unit);
   if (status != SUCCESS)
      return status;

   status = v2_close_os_file(unit);
   if (status != SUCCESS)
      return status;

   if (bufstate->buffer != NULL)	/* Free the buffer area */
      free(bufstate->buffer);

   if (CURRENT_IP_VALUE(LABELS) != 0)
      free(CURRENT_IP_VALUE(LABELS));
   CURRENT_I_VALUE(LBLALLOC) = 0;

   CURRENT_I_VALUE(FLAGS) &= ~OPEN;

   return SUCCESS;
}
