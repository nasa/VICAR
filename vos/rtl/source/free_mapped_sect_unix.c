#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>

/* Deletes the virtual address space associated with an array file */

int v2_free_mapped_sect(struct arraystate *arraystate)
{
   int status;

#if MMAP_AVAIL_OS

   errno = 0;
   status = munmap(arraystate->start, (size_t)arraystate->size);

   if (status == 0)
      return SUCCESS;
   else
      return errno;

#else				/* nothing to unmap */

   return SUCCESS;

#endif

}
