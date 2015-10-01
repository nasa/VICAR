#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include <unistd.h>
#include <errno.h>

/* Increases the allocation size of the file associated with 'diskstate'. */

int v2_extend_disk_file(
   struct diskstate *diskstate,
   V2_OFFSET amount
)

{

#if FTRUNCATE_AVAIL_OS
   if (V2_FTRUNCATE(diskstate->channel, amount*diskstate->blocksize) != 0)
      return errno;
#endif

   diskstate->allocation = amount;

   return SUCCESS;

}
