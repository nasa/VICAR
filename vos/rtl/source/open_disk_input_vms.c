#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <rms.h>

/* Open an input file on disk */

/* New-style temporary files are handled by the v2_expand_filename()	*/
/* call.  Old-style ones are handled by the dna (default filename)	*/
/* fields of the FAB structure.						*/

int v2_open_disk_input(unit, bufstate)
int unit;
struct bufstate *bufstate;
{
   int i;
   struct FAB fab;
   struct XABFHC xab;
   int l,j,k;
   int channel;
   struct NAM nam;
   struct diskstate *diskstate;
   int status;
   char filename[MAX_STRING_SIZE+1];
   char *v2_expand_filename();

   diskstate = &bufstate->devstate.dev.disk;

/* initialize the rms fab control block. */

   fab = cc$rms_fab;
   nam = cc$rms_nam;
   fab.fab$l_dna = default_file_name;
   fab.fab$b_dns = strlen(default_file_name);
   fab.fab$l_fna = v2_expand_filename(CURRENT_S_VALUE(NAME), FALSE, &status);
   fab.fab$b_fns = strlen(fab.fab$l_fna);
   fab.fab$b_fac = FAB$M_BIO | FAB$M_GET;
   if (EQUAL(CURRENT_S_VALUE(OP),"UPDATE"))
      fab.fab$b_fac |= FAB$M_PUT;
   fab.fab$l_fop = FAB$M_UFO;
   fab.fab$l_xab = &xab;
   fab.fab$l_nam = &nam;           /* Link the NAM to the FAB */
   nam.nam$l_rsa = filename;
   nam.nam$b_rss = MAX_STRING_SIZE;

   xab = cc$rms_xabfhc;            /* Initialize the xab. */

/* open the file */

   status = sys$open(&fab);

   *(filename + nam.nam$b_rsl) = '\0';	/* Make returned name a C string */
   if (nam.nam$b_rsl != 0)		/* make sure name is present */
     add_msg_current_table(filename,NAME,current_table[unit],
				  default_table);

   if (status != RMS_SUCCESS)
      return status;

   diskstate->name = CURRENT_S_VALUE(NAME);

   bufstate->devstate.device_type = DEV_DISK;

   diskstate->channel = fab.fab$l_stv;		/* save channel */

   if (bufstate->recsize == 0) {
      bufstate->recsize = fab.fab$w_mrs;		/* save recsize */
      CURRENT_I_VALUE(RECSIZE) = fab.fab$w_mrs;
   }

   bufstate->blocksize = fab.fab$w_bls;	      /* block size... better be 512! */
   diskstate->blocksize = fab.fab$w_bls;
   diskstate->allocation = fab.fab$l_alq;	/* allocation size of file */

   if (xab.xab$l_ebk == 0)		/* eof_record is in terms of bytes */
      bufstate->eof_record = 0;		/* until v2_initialize_buffer */
   else
      bufstate->eof_record =
	((xab.xab$l_ebk-1) * (V2_OFFSET)diskstate->blocksize + xab.xab$w_ffb);

/* Acquire an event flag */
/* An event flag is not obtained for read-only files, since the flag is	*/
/* not used on normal reads, and this allows removing the 32 open file	*/
/* limit.  If you want to do read-ahead caching (BUF_CACHE flag),	*/
/* you'll want to get a flag for reads too.				*/

   if (!EQUAL(CURRENT_S_VALUE(OP),"READ")) {
      status = lib$get_ef(&diskstate->io_event);
      if (status != SUCCESS)
	 return UNABLE_TO_ACQUIRE_AN_EVENT_FLAG;
   }
   else
      diskstate->io_event = 0;

   return SUCCESS;
}
