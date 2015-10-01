#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Sets the file offset, which is the # of bytes to skip in the file	*/
/* before the logical beginning of it (e.g. to accomodate a PDS label	*/
/* in front of the VICAR one).						*/
/*									*/
/* NOTE:  ONLY UNIX DISK FILES ARE SUPPORTED AT THIS TIME!!!		*/
/* And Array I/O files are not supported either.			*/
/*									*/
/* The function returns SUCCESS if it could set the offset, and FAILURE	*/
/* otherwise (i.e. wrong kind of device).  No error message is		*/
/* generated; that's the responsibility of the caller.			*/

int v2_set_file_offset(int unit, V2_OFFSET new_offset)
{
#if VMS_OS
   return FAILURE;			/* Not supported on VMS */
#else
   struct bufstate *bufstate;
   struct devstate *devstate;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   devstate = &bufstate->devstate;

   if (devstate->device_type != DEV_DISK)
      return FAILURE;			/* Only supported on disks */

   if (CURRENT_PP_VALUE(ADDRESS) != NULL)
      return FAILURE;			/* Array I/O not supported */

   devstate->file_offset = new_offset;

   return SUCCESS;
#endif
}

