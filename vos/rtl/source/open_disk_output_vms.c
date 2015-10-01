#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <rms.h>

/* Open an output file on disk */

/* New-style temporary files are handled by the v2_expand_filename()	*/
/* call.  Old-style ones are handled by the dna (default filename)	*/
/* fields of the FAB structure.						*/

int v2_open_disk_output(unit, bufstate)
int unit;
struct bufstate *bufstate;
{
   V2_OFFSET nr;
   int new_file;
   struct FAB fab, fab_alt;
   struct XABFHC xab;
   V2_OFFSET cursize, newsize;
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
   fab.fab$l_fna = v2_expand_filename(CURRENT_S_VALUE(NAME), TRUE, &status);
   fab.fab$b_fns = strlen(fab.fab$l_fna);
   fab.fab$w_mrs = CURRENT_I_VALUE(RECSIZE);
   if (CURRENT_I_VALUE(RECSIZE) > MAX_DISK_RECSIZE ||
	(CURRENT_I_VALUE(RECSIZE))&1 != 0)
      fab.fab$w_mrs = 512;    /* fix RMS problems with big & odd-length files */
   fab.fab$w_bls = CURRENT_I_VALUE(BUFSIZE);
   fab.fab$b_rfm = FAB$C_FIX;			/* record format= fixed */
   fab.fab$b_fac = FAB$M_BIO | FAB$M_GET | FAB$M_PUT;
   fab.fab$b_org = FAB$C_SEQ;
   fab.fab$l_nam = &nam;			/* Link the NAM to the FAB */

   fab_alt = fab;
   fab_alt.fab$l_xab = &xab;
   fab_alt.fab$l_fop = FAB$M_CIF;
   fab.fab$l_fop = FAB$M_UFO | FAB$M_NAM | FAB$M_SUP;

   nam.nam$l_rsa = filename;
   nam.nam$b_rss = MAX_STRING_SIZE;

   xab = cc$rms_xabfhc;				/* Initialize the xab. */

/* Open the file */

   new_file = FALSE;

   status = sys$create(&fab_alt); /* FAB$M_CIF set, so opens the file instead */
				  /* of creating it if it already exists */

   if (status != RMS_SUCCESS && status != RMS$_CREATED) {

      fab_alt.fab$l_fop &= ~FAB$M_CIF;	/* Try creating it from scratch */
      status = sys$create(&fab_alt);

      if (status != RMS_SUCCESS && status != RMS$_CREATED) {
         *(filename + nam.nam$b_rsl) = '\0'; /* Make returned name a C string */
         if (nam.nam$b_rsl != 0)		/* make sure name is present */
            add_msg_current_table(filename,NAME,
					 current_table[unit],default_table);
	 return status;
      }
      new_file = TRUE;
   }

   if (status == RMS$_CREATED)
      new_file = TRUE;

   bufstate->devstate.device_type = DEV_DISK;

   sys$close(&fab_alt);	/* This open and close is just to check existence */
			/* and file size. */

   *(filename + nam.nam$b_rsl) = '\0';	/* Make returned name a C string */
   if (nam.nam$b_rsl != 0)		/* make sure name is present */
      add_msg_current_table(filename,NAME,current_table[unit],
				   default_table);
   diskstate->name = CURRENT_S_VALUE(NAME);
   fab.fab$b_fns = nam.nam$b_rsl;      /* Update the fab file name and length */
   fab.fab$l_fna = CURRENT_S_VALUE(NAME);

/* If the file exists, then check the size of the file.  If it is >= the */
/* size required then re-open it for qio processing else delete it and	 */
/* create a file of the correct size.					 */

   bufstate->blocksize = fab_alt.fab$w_bls;  /* block size... better be 512! */
   diskstate->blocksize = fab_alt.fab$w_bls;

   cursize = fab_alt.fab$l_alq;		/* size of current file */

/* Determine file allocation size to use */

   newsize = 0;

   if (CURRENT_I_VALUE(N2) != 0) {	/* compute the required file size */
      nr = (CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3)) + CURRENT_I_VALUE(NLB);
      newsize = CEIL(CURRENT_I_VALUE(RECSIZE)*nr +
				CURRENT_I_VALUE(LBLSIZE) + EXTRA_FILE_SIZE,
		     diskstate->blocksize);
   }
   else if (primary_input_open) {
      /* if N2 is 0, it means the primary input is unlabeled and U_N2 was not */
      /* input for the current file, so use the primary file's SIZE	    */

      if (primary_allocation_size != 0) {
	 newsize = primary_allocation_size +
		   CEIL(PRIMARY_I_VALUE(LBLSIZE)+EXTRA_FILE_SIZE,
		        diskstate->blocksize);
      }
      else {
	 newsize=CEIL(primary_eof_record * CURRENT_I_VALUE(RECSIZE)
			 + PRIMARY_I_VALUE(LBLSIZE) + EXTRA_FILE_SIZE,
		      diskstate->blocksize);
      }
   }
   if (newsize == 0) {	/* there is no SIZE so must do with present file. */
      if (new_file)
	 return IMAGE_SIZE_REQUIRED;
      if (CURRENT_I_VALUE(N1) == 0)
	 CURRENT_I_VALUE(RECSIZE) = fab_alt.fab$w_mrs;
      newsize = cursize;
   }

   if (newsize > cursize) {		/* Create a file of the required size */
      fab.fab$l_alq = newsize;		/* set the req size in fab */
      status = sys$create(&fab);	/* create a file of the req size */
      if (status != RMS_SUCCESS && status != RMS$_SUPERSEDE)
	 return status;
   }
   else {	/* Existing file is ok, so just open it for qio processing. */
      fab.fab$l_fop |= FAB$M_NAM;
      status = sys$open(&fab);
      if (status != RMS_SUCCESS)
	 return status;
   }
   diskstate->allocation = fab.fab$l_alq;

   bufstate->eof_record = 0;		/* we're starting a new file */

   diskstate->channel = fab.fab$l_stv;		/* save channel */

/* Acquire an event flag */

   status = lib$get_ef(&diskstate->io_event);
   if (status != SUCCESS)
      return UNABLE_TO_ACQUIRE_AN_EVENT_FLAG;

   return SUCCESS;
}
