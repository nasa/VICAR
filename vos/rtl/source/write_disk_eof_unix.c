#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#include <errno.h>

/* Updates the end-of-file in the disk header to be 'last_byte'. */

int v2_write_disk_eof(int UNUSED(unit), struct diskstate *diskstate, 
		      V2_OFFSET last_byte, int recsize)
{

   errno = 0;

#if FTRUNCATE_AVAIL_OS
   if (V2_FTRUNCATE(diskstate->channel, last_byte) != 0)
      return errno;
#endif

   diskstate->allocation = CEIL(last_byte, recsize);

   return SUCCESS;
}
