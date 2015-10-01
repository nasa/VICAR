#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <ssdef.h>

/* Wait for and return the status of a pending io on the file associated */
/* with 'state'.							 */

int io_complete_check(devstate)
struct devstate *devstate;
{
   int code;

   if (!devstate->async_pending)
      return SUCCESS;

   switch (devstate->device_type) {

      case DEV_DISK:
	 if (devstate->dev.disk.io_event == 0)
	    return SUCCESS;		/* whoops, no async if no event flag */
	 code = sys$synch(devstate->dev.disk.io_event,&devstate->dev.disk.iosb);
	 if (code != SUCCESS)
	    return IO_WAIT_FAIL;
	 devstate->transfer_count = devstate->dev.disk.iosb.transfer_count;
	 code = devstate->dev.disk.iosb.status;
	 devstate->dev.disk.iosb.status = SUCCESS; /* no error if called again*/
	 break;

      case DEV_TAPE:
	 if (devstate->dev.tape.io_event == 0)
	    return SUCCESS;		/* whoops, no async if no event flag */
	 code = sys$synch(devstate->dev.tape.io_event,&devstate->dev.tape.iosb);
	 if (code != SUCCESS)
	    return IO_WAIT_FAIL;
	 devstate->transfer_count += devstate->dev.tape.iosb.transfer_count;
	 code = devstate->dev.tape.iosb.status;
	 devstate->dev.tape.iosb.status = SUCCESS; /* no error if called again*/
	 break;

      case DEV_ANSI:
	 if (devstate->dev.ansi.io_event == 0)
	    return SUCCESS;		/* whoops, no async if no event flag */
	 code = sys$synch(devstate->dev.ansi.io_event,&devstate->dev.ansi.iosb);
	 if (code != SUCCESS)
	    return IO_WAIT_FAIL;
	 devstate->transfer_count += MIN(devstate->dev.ansi.iosb.transfer_count,
					devstate->dev.ansi.blocksize);
	 code = devstate->dev.ansi.iosb.status;
	 devstate->dev.ansi.iosb.status = SUCCESS; /* no error if called again*/
	 break;

      case DEV_MEMORY:
	 code = SUCCESS;
	 break;

      case DEV_ARRAY:
	 code = SUCCESS;
	 break;

      case DEV_DECNET:
	 code = NOT_IMPLEMENTED;
	 break;
   }

   devstate->async_pending = FALSE;

   if (code != SS$_NORMAL) {
      if (code == SS$_ENDOFFILE)
	 if (devstate->transfer_count == 0)
	    return END_OF_FILE;		/* EOF only if nothing transferred */
	 else
	    return SUCCESS;
      return code;
   }

   return SUCCESS;
}
