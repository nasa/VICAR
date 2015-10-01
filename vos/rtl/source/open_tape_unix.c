#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <errno.h>
#include <sys/types.h>
#if FCNTL_AVAIL_OS
#include <fcntl.h>
#else
#include <sys/file.h>
#endif
#include <sys/ioctl.h>
#include <sys/mtio.h>

/* Open a tape file under Unix */

int v2_open_tape(int unit, struct bufstate *bufstate, int in_index, int filenr)
{
   int once_opened, current_file;
   struct tapestate *tapestate;
   int u_fil, def_fil, virgin_file;
   int index, file = 0;
   char tae_name[MAX_FILE_NAME_SIZE+1];
   struct mtget info;
   int status;
   char device_name[MAX_FILE_NAME_SIZE+1];

   bufstate->devstate.device_type = DEV_TAPE;
   tapestate = &bufstate->devstate.dev.tape;
   bufstate->eof_record = -1;		/* unknown */

   if (CURRENT_PP_VALUE(ADDRESS) != NULL)
      return ARRAY_IO_NOT_ALLOWED;

   bufstate->flags |= BUF_NOPREREAD;
   CURRENT_I_VALUE(FLAGS) |= SEQUENTIAL_DEVICE;

   tapestate->tindex = in_index;
   index = in_index;

   once_opened = active_units[unit].was_once_open;
   current_file = i_file[in_index];
   u_fil = CURRENT_I_VALUE(U_FILE);    /* Programmer specified file at xvopen */
   def_fil = (int)default_table[U_FILE].ivalue;	/* Default value of u_fil. */
   virgin_file = ((current_file==1) && (i_rec[in_index]==1));

/* Don't permit update to a tape. */

   if (EQUAL(CURRENT_S_VALUE(OP), "UPDATE"))
      return ILLEGAL_TAPE_OPERATION;

/* Don't permit random access to a tape unless it's READ */

   if (EQUAL(CURRENT_S_VALUE(METHOD), "RANDOM") &&
       !EQUAL(CURRENT_S_VALUE(OP), "READ"))
      return ILLEGAL_TAPE_METHOD;

   if (!once_opened) {			/* Determine desired tape position. */
      if (filenr==0) {			/* If no specified file on command. */
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
            file = filenr;
         else
            if (u_fil==0)		/* Programmer indicated next file. */
               file = current_file;
            else 
               if (u_fil>0)		/* Programmer indicated file.    */
                  file = u_fil;
      }
   }
   else {				/* File opened previously.    */
      if (filenr==0) {			/* If no specified file on command. */
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
            file = filenr;
         else
            if (u_fil==0)		/* Programmer indicated next file. */
               file = current_file+1;
            else 
               if (u_fil>0)		/* Programmer indicated file. */
                  file = u_fil;
      }
   }

   active_units[unit].was_once_open = TRUE;

/* Determine the device name of the tape unit; i_crack accepts a	*/
/* tape table item and returns the tae and operating system names.	*/

   v2_i_crack(i_tape[index], tae_name, device_name);

   if (i_file[index] < 0)		/* Tape is not mounted */
      return DEVICE_NOT_MOUNTED;

/* Open tape */

   tapestate->channel = open(device_name, O_RDONLY, 0666);
   if (tapestate->channel == -1) {
      v2_close_down(unit);			/* If open fails, clean up */
      return errno;
   }

/* check that the device is really a tape */
   if (ioctl(bufstate->devstate.dev.tape.channel, MTIOCGET, &info) == -1) {
      v2_close_down(unit);
      return errno;
   }

   i_open[index] = TRUE;		/* Indicate that the tape is open. */

   CURRENT_I_VALUE(FLAGS) |= UNIT_IS_TAPE;

/* Position tape to desired file. */

   status = v2_i_position_tape(tapestate->channel, index, file);
   if (status == EOF)
      return END_OF_VOLUME;
   if (status != SUCCESS)
      return status;

/* Since the tape driver appears to write double-EOFs whenever we	*/
/* move the tape when it's opened for write, we opened it for read	*/
/* originally to position it.  Now, close it and re-open for write.	*/
/* Note that this doesn't work on a rewinding device!			*/

   if (EQUAL(CURRENT_S_VALUE(OP), "WRITE")) {
      close(tapestate->channel);
      tapestate->channel = open(device_name, O_WRONLY, 0666);
      if (tapestate->channel == -1) {
         v2_close_down(unit);
         return errno;
      }
   }

/* If there is no input label the tape blocksize must be determined */

   if (EQUAL(CURRENT_S_VALUE(OP), "WRITE")) {

      CURRENT_I_VALUE(BUFSIZE) = default_table[BUFSIZE].ivalue;

      /* See if the user has set the block size on the tape mount. */

      status = v2_det_tape_blksize(unit, DONT_READ,index,file);
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
      status = v2_det_tape_blksize(unit,READ_OK,index,file);
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

int v2_open_tape(int unit, struct bufstate *bufstate, int in_index, int filenr)
{
   return NO_TAPE_SUPPORT;
}

#endif

