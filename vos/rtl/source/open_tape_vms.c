#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <descrip.h>
#include <ssdef.h>

/* Open a tape file under VMS */

int v2_open_tape(unit, bufstate, h1, h2)
int unit;
struct bufstate *bufstate;
int h1, h2;
{
   int once_opened, current_file;
   struct tapestate *tapestate;
   int u_fil, def_fil, virgin_file;
   int index, file;
   char tae_name[MAX_FILE_NAME_SIZE+1];
   char device_name[MAX_FILE_NAME_SIZE+1];
   int status;

   $DESCRIPTOR(device,device_name);

   bufstate->devstate.device_type = DEV_TAPE;
   tapestate = &bufstate->devstate.dev.tape;
   bufstate->eof_record = -1;		/* unknown */

   if (CURRENT_PP_VALUE(ADDRESS) != NULL)
      return ARRAY_IO_NOT_ALLOWED;

   bufstate->flags |= BUF_NOPREREAD;
   CURRENT_I_VALUE(FLAGS) |= SEQUENTIAL_DEVICE;

   tapestate->tindex = h1;
   index = h1;

   once_opened = active_units[unit].was_once_open;
   current_file = i_file[h1];
   u_fil = CURRENT_I_VALUE(U_FILE);   /* Programmer specified file at xvopen. */
   def_fil = (int)default_table[U_FILE].ivalue;	/* Default value of u_fil. */
   virgin_file = ((current_file==1) && (i_rec[h1]==1));

/* Don't permit update to a tape. */

   if (EQUAL(CURRENT_S_VALUE(OP), "UPDATE"))
      return ILLEGAL_TAPE_OPERATION;

/* Don't permit random access to a tape unless it's READ */

   if (EQUAL(CURRENT_S_VALUE(METHOD), "RANDOM") &&
       !EQUAL(CURRENT_S_VALUE(OP), "READ"))
      return ILLEGAL_TAPE_METHOD;

   if (!once_opened) {			/* Determine desired tape position. */
      if (h2==0) {			/* If no specified file on command. */
	 if (u_fil == def_fil)		/* If no programmer file specified. */
	    if (virgin_file)
	       file = current_file;
	    else
	       file=current_file+1;

	 else
	    if (u_fil == 0)		/* Programmer indicated next file. */
	       if (virgin_file)
		  file=current_file;
	       else
		  file=current_file+1;

	    else
	       if (u_fil>0)		/* Programmer indicated file */
		  file = u_fil;
      }  
      else {				/* Command line indicates file.   */
	 if (u_fil==def_fil)		/* No programmer file.          */
	    file = h2;
	 else
	    if (u_fil==0)		/* Programmer indicated next file. */
	       file = current_file;
	 else 
	    if (u_fil>0)		/* Programmer indicated file.    */
	       file = u_fil;
      }
   }
   else {				/* File opened previously.    */
      if (h2==0) {			/* If no specified file on command. */
	 if (u_fil == def_fil)		/* If no programmer file specified.*/
	    file = current_file;

      else
	 if (u_fil == 0)		/* Programmer indicated next file */
	    file=current_file+1;
	 else
	    if (u_fil>0)		/* Programmer indicated next file */
	       file = u_fil;
      }
      else {				/* Command line indicates file. */
	 if (u_fil==def_fil)		/* No programmer file. */
	    file = h2;
	 else
	    if (u_fil==0)		/* Programmer indicated next file. */
	       file = current_file+1;
	    else 
	       if (u_fil>0)		/* Programmer indicated file. */
		  file = u_fil;
      }
   }

   active_units[unit].was_once_open = TRUE;

/* Determine the VMS device name of the tape unit; i_crack accepts a	*/
/* tape table item and returns the tae and vms names.			*/

   i_crack(i_tape[index], tae_name, device_name);

   if (i_file[index] < 0)		/* Tape is not mounted */
      return DEVICE_NOT_MOUNTED;

/* Acquire an event flag */
/* An event flag is not obtained for read-only files, since the flag is	*/
/* not used on normal reads, and this allows removing the 32 open file	*/
/* limit.  If you want to do read-ahead caching (BUF_CACHE flag),	*/
/* you'll want to get a flag for reads too.				*/

   if (!EQUAL(CURRENT_S_VALUE(OP),"READ")) {
      status = lib$get_ef(&tapestate->io_event);
      if (status != SUCCESS)
	 return UNABLE_TO_ACQUIRE_AN_EVENT_FLAG;
   }
   else
      tapestate->io_event = 0;

   device.dsc$w_length = strlen(device_name);

/* Assign the tape unit. */

   tapestate->channel = 0;
   status = sys$assign(&device,&tapestate->channel,NULL,NULL);
   if (status != SUCCESS) {
      v2_close_down(unit);		/* If open fails, clean up. */
      return status;
   }

   i_open[index] = TRUE;		/* Indicate that the tape is open. */

   CURRENT_I_VALUE(FLAGS) |= UNIT_IS_TAPE;

/* Position tape to desired file */

   status = i_position_tape(tapestate->channel, index, file);
   if (status == SS$_ENDOFVOLUME)
      return END_OF_VOLUME;
   if (status != SUCCESS)
       return status;

/* If there is no input label the tape blocksize must be determined */

   if (EQUAL(CURRENT_S_VALUE(OP), "WRITE")) {

      CURRENT_I_VALUE(BUFSIZE) = default_table[BUFSIZE].ivalue;

      /* See if the user has set the block size on the tape mount. */

      status = v2_det_tape_blksize(unit, DONT_READ);
      if (status != SUCCESS)
	 return status;

      if ((CURRENT_I_VALUE(BUFSIZE) != default_table[BUFSIZE].ivalue) &&
	  (CURRENT_I_VALUE(BUFSIZE) % CURRENT_I_VALUE(RECSIZE) != 0))
	 return BUF_NOT_MULT_OF_REC;

      /* If no blocking is requested, then set BUFSIZE = RECSIZE */

      if (CURRENT_I_VALUE(FLAGS) & NOBLOCK) {
	 bufstate->blocksize = CURRENT_I_VALUE(RECSIZE);
	 tapestate->blocksize = CURRENT_I_VALUE(RECSIZE);
	 CURRENT_I_VALUE(BUFSIZE) = CURRENT_I_VALUE(RECSIZE);
      }
   }
   else {						/* OP == READ */
      status = v2_det_tape_blksize(unit,READ_OK);
      if (status == END_OF_FILE)
	 return END_OF_VOLUME;
      if (status != SUCCESS)
	 return status;
      if (CURRENT_I_VALUE(FLAGS) & NO_LABELS) {
	 status = v2_det_tape_recsize(unit);
	 if (status != SUCCESS) 
	    return status;
      }
   }

   return SUCCESS;
}

#else

int v2_open_tape()
{
   return NO_TAPE_SUPPORT;
}

#endif

