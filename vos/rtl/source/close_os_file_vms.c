#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

/* Perform the actual OS close of the file */

int close_os_file(unit)
int unit;
{
   int status;

   struct bufstate *bufstate;
   struct devstate *devstate;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   devstate = &bufstate->devstate;

   switch (devstate->device_type) {

      case DEV_DISK:
	 sys$dassgn(devstate->dev.disk.channel);     /* Free the channel */
	 if (devstate->dev.disk.io_event != 0)
	    lib$free_ef(&devstate->dev.disk.io_event); /* Free the event flag */
	 break;

      case DEV_TAPE:
	 sys$dassgn(devstate->dev.tape.channel);     /* Free the channel */
	 if (devstate->dev.tape.io_event != 0)
	    lib$free_ef(&devstate->dev.tape.io_event); /* Free the event flag */
	 break;

      case DEV_ANSI:
	 sys$dassgn(devstate->dev.ansi.channel);     /* Free the channel */
	 if (devstate->dev.ansi.io_event != 0)
	    lib$free_ef(&devstate->dev.ansi.io_event); /* Free the event flag */
	 break;

      case DEV_MEMORY:
	 bufstate->buffer = NULL;
	 break;

      case DEV_ARRAY:
	 status = free_mapped_sect(&devstate->dev.array);
	 sys$dassgn(devstate->dev.array.disk.channel);    /* Free the channel */
	 if (devstate->dev.array.disk.io_event != 0)
	    lib$free_ef(&devstate->dev.array.disk.io_event); /*Free event flag*/
	 bufstate->buffer = NULL;
	 if (status != SUCCESS)
	    return CANNOT_DELETE_SECTION;
	 break;

      case DEV_DECNET:
	 break;
   }

   return SUCCESS;
}
