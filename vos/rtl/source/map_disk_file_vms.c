#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <secdef.h>
#include <ssdef.h>

/* This routine will map a section of memory to a disk file, that is, make */
/* the file the paging file for the memory section.			   */

int v2_map_disk_file(unit)
int unit;
{
   struct bufstate *bufstate;
   struct arraystate *arraystate;
   int flags, cluster_size;
   V2_OFFSET i, amount;
   V2_OFFSET bytes_in_section, eof_loc;
   char *s[2], *r[2];
   int status;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   arraystate = &bufstate->devstate.dev.array;

   if (bufstate->devstate.device_type != DEV_DISK)
      return ARRAY_IO_NOT_ALLOWED;

   r[0] = r[1] = 10;	/* Having these addresses the same causes the	*/
			/* program region to be expanded (10 is just an	*/
			/* arbitrary number < p1 region			*/

   flags = SEC$M_EXPREG;	/* We want to expand the program region */

   bytes_in_section = arraystate->disk.allocation * arraystate->disk.blocksize;

/* If OP is READ, section is read only, plus there is no need to */
/* compute the number of pages needed.				 */

   if (!EQUAL(CURRENT_S_VALUE(OP),"READ")) {
      if (CURRENT_I_VALUE(N2) != 0) {	/* compute # of bytes needed for file */
	 i = (CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3)) + CURRENT_I_VALUE(NLB);
	 eof_loc = MAX(CURRENT_I_VALUE(LBLSIZE)+CURRENT_I_VALUE(EOL_SIZE),
		       CURRENT_I_VALUE(RECSIZE));
	 eof_loc += i * CURRENT_I_VALUE(RECSIZE) + EXTRA_FILE_SIZE;
	 if (EQUAL(CURRENT_S_VALUE(OP),"WRITE") &&     /* include room for */
		    primary_input_open &&	      /* primary input labels */
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

      status = v2_write_disk_eof(unit, &arraystate->disk, eof_loc,
			      CURRENT_I_VALUE(RECSIZE));
      if (status != SUCCESS)
	 return status;
      bytes_in_section = eof_loc;

      if (EQUAL(CURRENT_S_VALUE(OP),"WRITE"))	/* DZRO for WRITE prevents it */
	 flags |= SEC$M_WRT | SEC$M_DZRO;	/* going to disk on the first */
      else if (EQUAL(CURRENT_S_VALUE(OP), "UPDATE")) /* access to a page      */
	 flags |= SEC$M_WRT;
   }

/* Now we do the actual mapping of the disk file to virtual memory.	*/
/* The cluster size seems to have a maximum of 127, so we shall set it	*/
/* to the MIN of the number of pages in the file and 127.		*/

   cluster_size = CEIL(bytes_in_section, arraystate->disk.blocksize);
   if (cluster_size > 127)
      cluster_size = 127;

   status = sys$crmpsc(r,s,0,flags,0,0,0,arraystate->disk.channel,  /* map it */
		       0,0,0,cluster_size);

   bufstate->devstate.device_type = DEV_ARRAY;
   arraystate->start = s[0];		/* save these so we can free the mem */
   arraystate->size = s[1] - s[0] + 1;	/* if there's an error 		     */

   if ((status != SS$_NORMAL) && (status != SS$_CREATED))
      return status;

   bufstate->buffer = s[0];
   bufstate->bufsize = s[1] - s[0] + 1;
   bufstate->blockno = 0;
   bufstate->nblocksinbuf = 1;
   bufstate->flags |= BUF_VALID;

   return SUCCESS;
}
