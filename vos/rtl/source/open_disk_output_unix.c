#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include <fcntl.h>
#include <errno.h>
#if FSTAT_BLKSIZE_OS
#include <sys/types.h>
#include <sys/stat.h>
#endif

int v2_open_disk_output(int unit, struct bufstate *bufstate)
{
   int status;
   char *fname;
#if FSTAT_BLKSIZE_OS
   struct V2_STATBUF statbuf;
#endif

/* Expand $var, ~user, and + filename forms to something open() can handle */

   fname = v2_expand_filename(CURRENT_S_VALUE(NAME), TRUE, &status);
   if (status != SUCCESS)
      return status;
   v2_add_msg_current_table(fname, NAME, current_table[unit], default_table);

/* Open the file */
/* Set protection to all r,w access.  The user can change this with umask. */

#if OPEN_PROTECT_OS
   bufstate->devstate.dev.disk.channel = V2_OPEN(CURRENT_S_VALUE(NAME),
					      O_RDWR|O_CREAT, 0666);
#else
   bufstate->devstate.dev.disk.channel = V2_OPEN(CURRENT_S_VALUE(NAME),
					      O_RDWR|O_CREAT);
#endif

   if (bufstate->devstate.dev.disk.channel == -1)
      return errno;

#if FSTAT_BLKSIZE_OS

   status = V2_FSTAT(bufstate->devstate.dev.disk.channel, &statbuf);
   if (status == 0)
      bufstate->blocksize = statbuf.st_blksize;
   else
      bufstate->blocksize = 512;		/* just in case... */
   if (bufstate->blocksize <= 1)
      bufstate->blocksize = 512;		/* just in case... */

#else

   bufstate->blocksize = 512;

#endif

   bufstate->devstate.dev.disk.blocksize = bufstate->blocksize;
   bufstate->devstate.dev.disk.pos = 0;
   bufstate->devstate.dev.disk.allocation = 0;

   return SUCCESS;
}
