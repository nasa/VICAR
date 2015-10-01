#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* Find a sub_string of 's' of the form "key=value" and return a pointer*/
/* to 'value' in the parameter of that name, and the length in vallen.	*/
/* The routine will return a pointer to the string AFTER the found item	*/
/* or NULL if key is not found; 'place'	will return the addr of found	*/
/* entry in 's'.  If 'key' is empty, then the next label item is	*/
/* returned, and the key for that item is returned in 'key'.		*/
/*									*/
/* For efficiency, we compute the length of the label once, and then	*/
/* update the length as we move through the label.  Previously, it was	*/
/* computing strlen(s) for *every* call to parse_label().  Since the	*/
/* label can be quite long, strlen on it can be very time-consuming.	*/

char *v2_find_entry(
   char *s,		/* In: String to search (the label) */
   char *key,		/* In/Out: key to search for (or found if blank) */
   char **value,	/* Out: Returned pointer to value */
   int *vallen,		/* Out: length of value */
   char **place		/* Out: pointer to key=value entry */
)

{
   int keylen, lbllen;
   char *sk, *ek, *sv, *ev;

   keylen = strlen(key);

   /* Look thru the string for 'key'. */

   lbllen = strlen(s);
   while (v2_parse_label(s, lbllen, &sk, &ek, &sv, &ev)) {
      if (keylen == ek-sk+1 || keylen==0) {		      /* same length */
         if (strncmp(key, sk, ek-sk+1) == 0 || keylen==0) {   /* same string */

            /* Found it! */

            *value = sv;
            *vallen = ev-sv+1;
            *place = sk;

	    if (keylen == 0) {	   /* Key blank, so return the one found */
               strncpy(key, sk, ek-sk+1);
               key[ek-sk+1] = '\0';
	    }

            return ev+1;
         }
      }
      lbllen -= (ev+1 - s);	/* adjust string length */
      s = ev+1;		/* go to next key (past end of value) */
   }
   *place = sk;		/* parse_label sets it to end of label on error */
   return NULL;
}
