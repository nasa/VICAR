#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include <fcntl.h>
#include <errno.h>
#if FSTAT_AVAIL_OS
#include <sys/types.h>
#include <sys/stat.h>
#else
#include seek_include
#endif

int v2_open_disk_input(int unit, struct bufstate *bufstate)
{
   int status;
   char *fname;
#if FSTAT_AVAIL_OS
   struct V2_STATBUF statbuf;
#else
   V2_OFFSET size;
#endif

/* Expand $var, ~user, and + filename forms to something open() can handle */

   fname = v2_expand_filename(CURRENT_S_VALUE(NAME), FALSE, &status);
   if (status != SUCCESS)
      return status;
   v2_add_msg_current_table(fname, NAME, current_table[unit], default_table);

/* Open the file */

   if (EQUAL(CURRENT_S_VALUE(OP),"UPDATE"))
      bufstate->devstate.dev.disk.channel =
				V2_OPEN(CURRENT_S_VALUE(NAME), O_RDWR);
   else
      bufstate->devstate.dev.disk.channel =
				V2_OPEN(CURRENT_S_VALUE(NAME), O_RDONLY);

   if (bufstate->devstate.dev.disk.channel == -1)
      return errno;

/* Get the total size of the file.  Eof_record is in terms of	*/
/* bytes until v2_initialize_buffer.				*/

#if FSTAT_AVAIL_OS

   status = V2_FSTAT(bufstate->devstate.dev.disk.channel, &statbuf);
   if (status != 0)
      return errno;
   bufstate->eof_record = statbuf.st_size;
  
#if FSTAT_BLKSIZE_OS
   bufstate->blocksize = statbuf.st_blksize;
   if (bufstate->blocksize <= 1)		/* just in case... */
      bufstate->blocksize = 512;
#else
   bufstate->blocksize = 512;
#endif

#else	/* !FSTAT_AVAIL_OS */

   bufstate->blocksize = 512;

   size = V2_LSEEK(bufstate->devstate.dev.disk.channel,(V2_OFFSET)0,SEEK_END);
   if (size == -1)
      return errno;
   bufstate->eof_record = size;
   size = V2_LSEEK(bufstate->devstate.dev.disk.channel, (V2_OFFSET)0, SEEK_SET);
   if (size == -1)
      return errno;

#endif

   bufstate->devstate.dev.disk.blocksize = bufstate->blocksize;
   bufstate->devstate.dev.disk.pos = 0;
   bufstate->devstate.dev.disk.allocation =
			CEIL(bufstate->eof_record, bufstate->blocksize);

   return SUCCESS;
}
