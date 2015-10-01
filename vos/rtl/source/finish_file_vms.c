#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

/* Finish up a file by writing the EOF mark, etc. */

int v2_finish_file(unit)
int unit;
{
   struct bufstate *bufstate;
   struct devstate *devstate;
   int status;
   int tindex;
   V2_OFFSET j;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   devstate = &bufstate->devstate;

   status = io_complete_check(devstate); /* wait for any pending reads/writes */
   if (status != SUCCESS)
      return status;

   switch (devstate->device_type) {	/* close the file */

#if RTL_USE_TAPE
      case DEV_TAPE:
	 tindex = devstate->dev.tape.tindex;
	 i_open[tindex] = FALSE;	/* Indicate not opened to TAE */
	 if (EQUAL(CURRENT_S_VALUE(OP), "WRITE")) {
	    status = double_eof(&devstate->dev.tape); /* Write the eof marks */
	    if (status != SUCCESS)
	       return status;
	 }
	 break;
#endif

      case DEV_ANSI:
	 break;

      case DEV_MEMORY:			/* No close processing necessary */
	 break;

      case DEV_ARRAY:		/* Falls through to the DISK close routine */

      case DEV_DISK:

/* If file is an output file, update the eof in the file header */

	 if (!EQUAL(CURRENT_S_VALUE(OP),"READ")) {

/* Calculate the total number of bytes in the file depending on the type of */
/* file we have.  Unlabeled output files check the actual number of records */
/* written, while input files and labeled files use file size info.         */

	    if ((CURRENT_I_VALUE(FLAGS) & NO_LABELS) &&
		     (EQUAL(CURRENT_S_VALUE(OP),"WRITE"))) {
	       j = bufstate->eof_record * bufstate->recsize;
	    }
	    else {
	       j = CURRENT_I_VALUE(N1) * CURRENT_I_VALUE(PIX_SIZE) +
			CURRENT_I_VALUE(NBB);
	       j =  j * ((CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3))
                    + CURRENT_I_VALUE(NLB))
                    + CURRENT_I_VALUE(LBLSIZE)
                    + CURRENT_I_VALUE(EOL_SIZE);
	    }
	    status = v2_write_disk_eof(unit, &devstate->dev.disk, j,
				    bufstate->recsize);
	    if (status != SUCCESS)
	       return status;
	 }
	 break;

      case DEV_DECNET:
	 break;
   }

   return SUCCESS;
}
