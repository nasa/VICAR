#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#include <errno.h>
#if MMAP_AVAIL_OS
#include <sys/types.h>
#include <sys/mman.h>
#endif

/* This routine will map a section of memory to a disk file, that is, make */
/* the file the paging file for the memory section.			   */

int v2_map_disk_file(int unit)
{
   struct bufstate *bufstate;
   struct arraystate *arraystate;
   int flags, prot;
   V2_OFFSET i, amount;
   V2_OFFSET bytes_in_section, eof_loc;
   char *hint_addr, *addr;
   int status;

   hint_addr = 0;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   arraystate = &bufstate->devstate.dev.array;

   if (bufstate->devstate.device_type != DEV_DISK)
      return ARRAY_IO_NOT_ALLOWED;

#if MMAP_AVAIL_OS			/* mmap() exists */

   flags = MAP_SHARED;
   prot  = PROT_READ;

   bytes_in_section = arraystate->disk.allocation * arraystate->disk.blocksize;

/* If OP is READ, section is read only, plus there is no need	*/
/* to compute the number of pages needed.			*/

   if (!EQUAL(CURRENT_S_VALUE(OP),"READ")) {
      if (CURRENT_I_VALUE(N2) != 0) {	/* compute # of bytes needed for file */
         i = (CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3)) + CURRENT_I_VALUE(NLB);
         eof_loc = MAX(CURRENT_I_VALUE(LBLSIZE)+CURRENT_I_VALUE(EOL_SIZE),
		       CURRENT_I_VALUE(RECSIZE));
         eof_loc += i * CURRENT_I_VALUE(RECSIZE) + EXTRA_FILE_SIZE;
         if (EQUAL(CURRENT_S_VALUE(OP),"WRITE") &&	/* include room for */
		primary_input_open &&			/* primary input lbls */
		!(PRIMARY_I_VALUE(FLAGS) & NO_LABELS)) {
            eof_loc += PRIMARY_I_VALUE(LBLSIZE) + PRIMARY_I_VALUE(EOL_SIZE);
         }

         /* Extend file before mapping if it's not big enough */

         if (eof_loc > bytes_in_section) {
            amount = CEIL(eof_loc, arraystate->disk.blocksize)
			- arraystate->disk.allocation;
            if (amount > 0) {
               status = v2_extend_disk_file(&arraystate->disk, amount);
               if (status != SUCCESS)
                  return status;
            }
         }
      }
      else {
         eof_loc = bytes_in_section;
      }

      /* make sure eof block is correct so we map right amount of	*/
      /* memory (map is only until eof block)				*/
      /* RECSIZE is not set up yet for UPDATE files, so we'll just skip this */

      if (!EQUAL(CURRENT_S_VALUE(OP),"UPDATE")) {
         status = v2_write_disk_eof(unit, &arraystate->disk, eof_loc,
			   CURRENT_I_VALUE(RECSIZE));
         if (status != SUCCESS)
            return status;
      }

      bytes_in_section = eof_loc;

      prot |= PROT_WRITE;

   }

   if (bytes_in_section != (V2_OFFSET) ((size_t)bytes_in_section))
      return FILE_TOO_BIG;

   errno = 0;
   addr = mmap(hint_addr, (size_t)bytes_in_section, prot, flags,
               arraystate->disk.channel, (V2_OFFSET)0);

   arraystate->start = addr;		/* save these so we can free the mem */
   arraystate->size  = bytes_in_section; /* if there's an error		     */

   if (addr == MAP_FAILED)
      return errno;

   bufstate->devstate.device_type = DEV_ARRAY;

   bufstate->buffer = addr;
   bufstate->bufsize = bytes_in_section;
   bufstate->blockno = 0;
   bufstate->nblocksinbuf = 1;
   bufstate->flags |= BUF_VALID;

   return SUCCESS;

#else /* MMAP_AVAIL_OS */

   return ARRAY_IO_NOT_ALLOWED;	/* if mmap() doesn't exist, return error */
		/* !!!! This should be changed to simulate array I/O !!!!*/

#endif

}
