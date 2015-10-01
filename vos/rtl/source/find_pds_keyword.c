#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include <ctype.h>
#include "strncasecmp.h"

/* Finds a PDS keyword in the given string, and returns a pointer to	*/
/* the value associated with the keyword.  Returns NULL if the keyword	*/
/* could not be found, or for fatal parsing errors.			*/

char *v2_find_pds_keyword(char *label, char *key)
{
   char *p;
   int keylen;

   keylen=strlen(key);
   p = label;

   while (*p != '\0') {

      /* Beginning of a line... skip blanks */

      while (*p == ' ')
         p++;

      /* Now see if we found the keyword */

      if (strncasecmp(p, key, keylen) == 0) {

         /* Must be followed by blank or =, else we found something bogus */

	 p += keylen;

	 while (*p == ' ')
	    p++;
	 if (*p == '=') {			/* Must have = */
	    p++;

	    /* Now find the value */

	    while (*p == ' ')
	       p++;

	    if (*p != '\n' && *p != '\r' && *p != '\0')	/* Found the value! */
	       return p;
         }
      }

      /* If any of the above tests failed, we look for \n in the	*/
      /* label and try again.						*/

      while (*p != '\n' && *p != '\0')
         p++;

      if (*p == '\n')
         p++;				/* start of next record */
   }

   return NULL;				/* not found */

}

