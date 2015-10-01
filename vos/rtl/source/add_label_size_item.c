#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include <stdio.h>

/* Return the label size keyword/value pair in the string 'ps'.  Result	*/
/* is blank-padded out to the length of SYSTEM_KEY_TEMPLATE to allow	*/
/* for modification later, if possible.  This routine checks for NULL	*/
/* or blank characters and ONLY pads over them.  If it finds a non-0,	*/
/* non-blank character, the extra blanks are suppressed.  This allows	*/
/* for non-standard files having shorter LBLSIZE fields - BUT, it means	*/
/* that the caller must 0 or blank the buffer before calling this	*/
/* routine (if there's no already-existing label).  The string plus	*/
/* one blank is always written, however, since LBLSIZE is more		*/
/* important than most other labels (this means that pathologically	*/
/* short LBLSIZE fields will still cause the first entry after LBLSIZE	*/
/* to be overwritten.  Don't do that; allow space for LBLSIZE to expand!*/
/* Note that there is *NO* null terminator put on the string.		*/

void v2_add_label_size_item(char *ps, int lblsize) 
{
   int i;

   sprintf(ps, "%s=%d ", SYSTEM_KEY, lblsize);
   for (i=strlen(ps); i<strlen(SYSTEM_KEY_TEMPLATE); i++) {
      if (ps[i] != '\0' && ps[i] != ' ')
         return;			/* Only overwrite blanks or nulls */
      ps[i] = ' ';
   }

   return;
}

