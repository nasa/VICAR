#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "ftnbridge.h"
#include <ctype.h>
#if VMS_OS
#include "nargs_vms.h"
#endif

/* Prints a line of data to the terminal and session log, using		*/
/* carriage control characters.  Also, translate nonprintable chars	*/
/* to '#' or '|'.							*/

/* THIS ROUTINE SHOULD NOT BE CALLED FROM NEW CODE.  It has been	*/
/* replaced by xvmessage, which should be called instead.  The only	*/
/* use for this routine is to print form feeds, which come out only	*/
/* in batch mode.  In interactive mode, they come out as blank lines.	*/
/* This keeps the terminal from scrolling the information away too	*/
/* quickly.  If used for this purpose, use it *ONLY* for the form feed.	*/
/* Print all other text with xvmessage.					*/

/* Column 1:	'0'	print blank line before the data.		*/
/*		'1'	do a form feed (batch only) before the data	*/
/*		' '	don't print the blank in the first column.	*/
/*	anything else	print the character in the first column.	*/


/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(qprint, QPRINT) (char *msg, int *len, ZFORSTR_PARAM)
/* char *msg;			in: string to print.			      */
/* int *len;			in: length to print.  0 means whole string */
{
   ZFORSTR_BLOCK
   int length;
   char c_msg[MAX_STRING_SIZE+1];

   zsfor2c(c_msg, MAX_STRING_SIZE, msg, &msg, 2, 1, 1, len);

#if VMS_OS		/* must allow 'len' to be optional for VMS... grrr */
   if (n_args() > 1)	/* hopefully this is temporary only */
      length = *len;
   else
      length = 0;
#else
   length = *len;
#endif

   zqprint(c_msg, length);

   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

void zqprint(char *msg, int len)
{
   int count;
   int length, i;
   char tempbuf[MAX_STRING_SIZE+1];

   if (len != 0)
      len = MIN(len, MAX_STRING_SIZE);
   else
      len = MAX_STRING_SIZE;

   if (*msg == '0') {
      zvmessage("","");			/* print blank line */
      msg++;				/* skip the '0' */
      len--;
   }
   else if (*msg == '1') {
      zvp("$RUNTYPE", tempbuf, &count);
      if (strcmp(tempbuf, "INTERACTIVE") == 0)
         zvmessage("","");		/* Interactive, blank line only */
      else
         zvmessage("\f","");		/* Batch, do the form feed */
      msg++;				/* skip the '1' */
      len--;
   }
   else if (*msg == ' ') {
      msg++;				/* Ignore (and skip) the blank */
      len--;
   }

   length = MIN(len, (int) strlen(msg));
   strncpy(tempbuf, msg, len);
   tempbuf[length] = '\0';

   for (i=0; i<length; i++) {
      if (!isprint(tempbuf[i])) {	/* non-printing char, so replace it */
	if ((unsigned char) (tempbuf[i]) >= 128)
            tempbuf[i] = '#';
         else if (tempbuf[i] != '\n' && tempbuf[i] != '\r' &&
		  tempbuf[i] != '\t' && tempbuf[i] != '\f')
            tempbuf[i] = '|';	/* not tab, newline, return, or form feed */
      }
   }

   zvmessage(tempbuf, "");

   return;
}

