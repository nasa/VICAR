#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include <ctype.h>

/* Make a guess at the format of a label item.  Possible formats are	*/
/* INT (if it's all numbers), REAL (if there's a decimal point) and	*/
/* STRING (if there's any non-numbers).  Note that DOUB is a valid	*/
/* label type, but this routine will never guess DOUB (it gets REAL	*/
/* instead).  The format is returned as a string.			*/
/* NOTE:  The returned string is in local static memory... so you'd	*/
/* better use the value or copy it elsewhere before calling this	*/
/* routine again!							*/

char *v2_determine_format(char *value)
{
   int real;
   char c;
   static char retval[10];
   int i;

   real = FALSE;

   for (i=0; i<strlen(value); i++) {
      c = value[i];
      if (isdigit(c) || isspace(c) || strchr("+-",c) != NULL)
         continue;			/* still valid for INT */
      if (strchr("eEdD.",c) != NULL) {
         real = TRUE;			/* must be a REAL */
         continue;
      }
      strcpy(retval, "STRING");		/* must be a STRING */
      return retval;
   }

   if (real)
      strcpy(retval, "REAL");
   else
      strcpy(retval, "INT");

   return retval;
}
