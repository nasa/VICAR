#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <ssdef.h>
#include <iodef.h>

/* Low-level tape positioning subroutines */

/* CAUTION: do not call these with an RMS record stream active;	*/
/* RMS does not know that we are doing a tape QIO and internal	*/
/* RMS buffers will get confused.				*/

#define TAPE_ERROR(code)	(((code)&1) != 1)

/************************************************************************/
/* Position tape to absolute file position.				*/
/************************************************************************/

int i_position_tape (channel, index, filenr)
int channel;		/* in: channel number		*/
int index;		/* in: index in tape tables	*/
int filenr;		/* in: file number		*/

{
   int skip;
   int code;

   if (i_file[index] == 0) {		/* tape position unknown */
      if (filenr == 0)			/* if asking for next file... */
         return SS$_TAPEPOSLOST;	/* a bad idea */
      else {
         code = i_rewind (channel, index);	/* get to known position */
         if (TAPE_ERROR(code))
            return code;
      }
   }

   if (filenr == 0)			/* use "next" file */
      filenr = i_nxt_file[index];

   if (filenr == 1) {			/* use first file, so rewind */
      code = i_rewind (channel, index);
      if (TAPE_ERROR(code))
         return code;
   }
   else {				/* use Nth file, so file skip */
      skip = filenr - i_file[index];	/* files to skip */
      if (skip <= 0)
         skip -= 1;			/* one more if reverse */
      code = i_space_file (channel, index, skip);
      if (TAPE_ERROR(code))
         return code;
      if (skip < 0) {			/* must skip forward over EOF */
         code = i_space_record (channel, index, 1);
         if (TAPE_ERROR(code) && code != SS$_ENDOFFILE)
            return code;
      }
   }
   i_nxt_file[index] = filenr;		/* new "next" file */

   return SUCCESS;
}

/************************************************************************/
/* Rewind tape.								*/
/************************************************************************/

int i_rewind (channel, index)
int channel;			/* in: VMS channel of tape	*/
int index;			/* in: tape index		*/

{
   int code;
   short iosb[4];

   code = sys$qiow (0, channel, IO$_REWIND|IO$M_NOWAIT, iosb,NULL,NULL, 
		    NULL,NULL,NULL,NULL,NULL,NULL);
   if (TAPE_ERROR(code))
      return code;
   if (TAPE_ERROR(iosb[0])) {
      i_rec[index] = 0;			/* tape position not known */
      i_file[index] = 0;
   }
   else {
      i_rec[index] = 1;			/* beginning of tape */
      i_file[index] = 1;
   }
   i_nxt_file[index] = 1;		/* new "next" file */

   return iosb[0];
}

/************************************************************************/
/* Space forward or back file marks.					*/
/************************************************************************/

int i_space_file (channel, index, files)
int channel;		/* in: VMS channel of tape		*/
int index;		/* in: index in tape tables		*/
int files;		/* in: number of files; negative means backwards */

{
   int code;
   short iosb[4];

   code = sys$qiow (0, channel, IO$_SKIPFILE, iosb,NULL,NULL,
		    files, NULL, NULL, NULL, NULL, NULL);
   if (TAPE_ERROR(code))
      return code;
   if (TAPE_ERROR(iosb[0]) && iosb[0] != SS$_ENDOFVOLUME) {
      i_rec[index] = 0;			/* tape position not known */
      i_file[index] = 0;
      return iosb[0];
   }

   if (files < 0) {
      i_rec[index] = 1;
      i_file[index] -= iosb[1];		/* new file number */
   }
   else {
      i_rec[index] = 1;
      i_file[index] += iosb[1];		/* new file number */
   }

   i_nxt_file[index] = i_file[index];	/* new "next" file */

   return iosb[0];
}

/************************************************************************/
/* Backspace tape one record.  Note that eof is considered SUCCESS.	*/
/************************************************************************/

int i_back_space (channel, index)
int channel;			/* in: tape channel number */
int index;			/* in: index in tape tables */

{
   int code;
   short iosb[4];

   code = sys$qiow (0, channel, IO$_SKIPRECORD, iosb,NULL,NULL,
		    -1, NULL, NULL, NULL, NULL, NULL);

   if (TAPE_ERROR(code))
      return code;

   if (TAPE_ERROR(iosb[0]) && iosb[0] != SS$_ENDOFFILE) {
      i_rec[index] = 0;			/* tape position not known */
      i_file[index] = 0;
      return iosb[0];
   }

   if (iosb[0] == SS$_ENDOFFILE) {
      i_rec[index] = 0;			/* record position not known */
      i_file[index]--;			/* previous file */
   }
   else					/* file position stays the same	*/
      i_rec[index] -= iosb[1];

   return SUCCESS;
}

/************************************************************************/
/* Space forward n records.						*/
/************************************************************************/

int i_space_record (channel, index, records)
int channel;			/* in: channel number of tape	*/
int index;			/* in: index in tape tables	*/
int records;			/* in: number of records	*/

{
   int code;
   short iosb[4];

   code = sys$qiow (0, channel, IO$_SKIPRECORD, iosb,NULL,NULL,
		    records, NULL, NULL, NULL, NULL, NULL);
   if (TAPE_ERROR(code))
      return code;
   if (TAPE_ERROR(iosb[0]) && iosb[0] != SS$_ENDOFFILE) {
      i_rec[index] = 0;			/* tape position not known */
      i_file[index] = 0;
      return iosb[0];
   }
   if (iosb[0] == SS$_ENDOFFILE) {
      i_rec[index] = 1;		/* before first record */
      i_file[index]++;		/* next file */
   }
   else				/* file position stays the same	*/
      i_rec[index] += iosb[1];

   return iosb[0];
}

#else

int i_position_tape()
{
   return NO_TAPE_SUPPORT;
}

int i_rewind ()
{
   return NO_TAPE_SUPPORT;
}

int i_space_file ()
{
   return NO_TAPE_SUPPORT;
}

int i_back_space ()
{
   return NO_TAPE_SUPPORT;
}

int i_space_record ()
{
   return NO_TAPE_SUPPORT;
}

#endif

