#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include seek_include
#include <errno.h>

int v2_write_disk(struct diskstate* disk, char *buf, V2_OFFSET block, 
		  V2_OFFSET nblocks, int UNUSED(async_flag), 
		  int *transfer_count,
		  V2_OFFSET file_offset)
{
   V2_OFFSET bytes;
   V2_OFFSET pos;

   pos = block * disk->blocksize + file_offset;

   if (pos != disk->pos) {			/* in the right place? */
      bytes = V2_LSEEK(disk->channel, pos, SEEK_SET);
      disk->pos = bytes;
      if (bytes != pos)
         return errno;
   }

   bytes = write(disk->channel, buf, (size_t)(nblocks*disk->blocksize));
   if (bytes == -1)
      return errno;
   disk->pos += bytes;

   *transfer_count = bytes;

   if (bytes != nblocks*disk->blocksize)
      return errno;

   return SUCCESS;
}
