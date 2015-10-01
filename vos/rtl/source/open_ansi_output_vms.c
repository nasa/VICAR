#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <rms.h>
#include <ssdef.h>

int v2_open_ansi_output(unit, bufstate)
int unit;
struct bufstate *bufstate;
{
   int nr;
   int new_file;
   struct FAB fab;
   int cursize, newsize;
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
   fab.fab$w_mrs = CURRENT_I_VALUE(RECSIZE);
   if (CURRENT_I_VALUE(RECSIZE) > MAX_ANSI_RECSIZE ||
				(CURRENT_I_VALUE(RECSIZE))&1 != 0)
      fab.fab$w_mrs = 512;     /* fix RMS problems with big & odd-len records */
   fab.fab$b_rfm = FAB$C_FIX;			/* record format= fixed */
   fab.fab$b_fac = FAB$M_BIO | FAB$M_GET | FAB$M_PUT;
   fab.fab$b_org = FAB$C_SEQ;
   fab.fab$l_fop = FAB$M_UFO | FAB$M_NAM;
   fab.fab$l_nam = &nam;			/* Link the NAM to the FAB */

   nam.nam$l_rsa = filename;
   nam.nam$b_rss = MAX_STRING_SIZE;

/* Open the file */

   new_file = FALSE;

   status = sys$create(&fab);	/* Create a new file, even if an old version */
				/* already exists... this prevents writing   */
				/* over other files after the one that exits.*/

/* If a create error (hopefully due to the record being bigger than the	*/
/* block), then try again with recsize==blocksize.			*/

   if (status == RMS$_CRE && fab.fab$l_stv == SS$_BADATTRIB) {
      fab.fab$w_mrs = fab.fab$w_bls;
      status = sys$create(&fab);
   }

   *(filename + nam.nam$b_rsl) = '\0';	/* Make returned name a C string */
   if (nam.nam$b_rsl != 0)		/* make sure name is present */
     add_msg_current_table(filename,NAME,current_table[unit],
				  default_table);

   if (status != RMS_SUCCESS && status != RMS$_CREATED)
      return status;

   if (status == RMS$_CREATED)
      new_file = TRUE;

   bufstate->devstate.device_type = DEV_ANSI;

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

   bufstate->eof_record = 0;		/* we're starting a new file */

   ansistate->channel = fab.fab$l_stv;		/* save channel */

   bufstate->flags |= BUF_NOPREREAD;
   CURRENT_I_VALUE(FLAGS) |= SEQUENTIAL_DEVICE;

/* Acquire an event flag */

   status = lib$get_ef(&ansistate->io_event);
   if (status != SUCCESS)
      return UNABLE_TO_ACQUIRE_AN_EVENT_FLAG;

   return SUCCESS;
}
