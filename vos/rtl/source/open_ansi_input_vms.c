#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <rms.h>

int v2_open_ansi_input(unit, bufstate)
int unit;
struct bufstate *bufstate;
{
   int i;
   struct FAB fab;
   int l,j,k;
   int channel;
   struct NAM nam;
   struct ansistate *ansistate;
   int status;
   char filename[MAX_STRING_SIZE+1];

   ansistate = &bufstate->devstate.dev.ansi;

   /* initialize the rms fab control block. */

   fab = cc$rms_fab;
   nam = cc$rms_nam;
   fab.fab$l_dna = default_file_name;
   fab.fab$b_dns = strlen(default_file_name);
   fab.fab$l_fna = CURRENT_S_VALUE(NAME);
   fab.fab$b_fns = strlen(CURRENT_S_VALUE(NAME));
   fab.fab$b_fac = FAB$M_BIO | FAB$M_GET;
   if (EQUAL(CURRENT_S_VALUE(OP),"UPDATE"))
      fab.fab$b_fac |= FAB$M_PUT;
   fab.fab$l_fop = FAB$M_UFO;
   fab.fab$l_nam = &nam;           /* Link the NAM to the FAB */
   nam.nam$l_rsa = filename;
   nam.nam$b_rss = MAX_STRING_SIZE;

   /* open the file */

   status = sys$open(&fab);

   *(filename + nam.nam$b_rsl) = '\0';	/* Make returned name a C string */
   if (nam.nam$b_rsl != 0)		/* make sure name is present */
      add_msg_current_table(filename,NAME,current_table[unit],
				   default_table);

   if (status != RMS_SUCCESS)
      return status;

   bufstate->devstate.device_type = DEV_ANSI;

   ansistate->channel = fab.fab$l_stv;		/* save channel */

   if (bufstate->recsize == 0) {
      bufstate->recsize = fab.fab$w_mrs;		/* save recsize */
      CURRENT_I_VALUE(RECSIZE) = fab.fab$w_mrs;
   }

   ansistate->phys_blocksize = fab.fab$w_bls;	/* physical block size */

/* Calculate the logical block size based on the phys block & record	*/
/* lengths.  If phys recsize doesn't divide evenly into phys blocksize,	*/
/* then the extra space is wasted, since DCL COPY, doesn't span	records	*/
/* on a tape.  The logical blocksize, used by read_rec & write_rec, is	*/
/* only the unwasted space.  Write_ansi and read_ansi make up for the	*/
/* wasted space without the higher level routines knowing.  The only	*/
/* thing they need to do is allocate the buffer a little bigger, by the	*/
/* amount in bufstate->buf_extra.					*/

   bufstate->blocksize = (fab.fab$w_bls / fab.fab$w_mrs) * fab.fab$w_mrs;
   if (bufstate->blocksize == 0)
      bufstate->blocksize = fab.fab$w_bls;	/* shouldn't ever happen! */
   ansistate->blocksize = bufstate->blocksize;

   bufstate->buf_extra = ansistate->phys_blocksize - ansistate->blocksize;

   bufstate->eof_record = -1;

   bufstate->flags |= BUF_NOPREREAD;
   CURRENT_I_VALUE(FLAGS) |= SEQUENTIAL_DEVICE;

/* Acquire an event flag */
/* An event flag is not obtained for read-only files, since the flag is	*/
/* not used on normal reads, and this allows removing the 32 open file	*/
/* limit.  If you want to do read-ahead caching (BUF_CACHE flag),	*/
/* you'll want to get a flag for reads too.				*/

   if (!EQUAL(CURRENT_S_VALUE(OP),"READ")) {
      status = lib$get_ef(&ansistate->io_event);
      if (status != SUCCESS)
	 return UNABLE_TO_ACQUIRE_AN_EVENT_FLAG;
   }
   else
      ansistate->io_event = 0;

   return SUCCESS;
}
