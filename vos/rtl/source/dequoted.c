#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* If the given string is enclosed in single quotes, this routine will	*/
/* strip them off.  Also, if it is in single quotes, any repeated	*/
/* single quotes will be returned as one single quote.  The enclosing	*/
/* quotes must be the first and last characters of the string.		*/
/* Returns a pointer to a local static memory area; the string must be	*/
/* not needed or copied elsewhere before dequoted() can be called again.*/

char *v2_dequoted(
   char *s,			/* In: string to dequote */
   int len			/* In: Length of string */
)

{
   int i, j;
   static char a[MAX_LABEL_VALUE_SIZE+1];

   if ((*s == '\'') && (*(s+len-1) == '\'')) {		/* In quotes */
      j = 0;
      for (i=1; i<len-1; i++) {
         a[j++] = s[i];
	 if (j == MAX_LABEL_VALUE_SIZE)
	    break;				/* too long for buffer */
         if ((s[i] == '\'') && (s[i+1] == '\''))
            i++;			/* skip the repeated quote */
      }
      a[j] = '\0';
   }
   else {		/* No quote pairs, maybe a single? (shouldn't happen) */
      if (*(s+len-1) == '\'')
         len--;
      if (*s == '\'') {
         s++;
         len--;
      }
      if (len > MAX_LABEL_VALUE_SIZE)		/* safety check */
         len = MAX_LABEL_VALUE_SIZE;
      strncpy(a, s, len);
      a[len] = '\0';
   }

   return a;
}
