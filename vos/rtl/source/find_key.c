#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* Given a system or history label bounded by 'start' and 'stop', */
/* look for a label item with the given key.  'start' and 'stop'  */
/* are updated to point at the given item, and 'value' and	  */
/* 'vallen' are set to the label value.  If the key is not found, */
/* both 'start' and 'stop' will point to the end of the range.	  */

int v2_find_key(
char **start,		/* in/out: starting point of search area/found item */
char **stop,		/* in/out: ending point of search area/found item */
char *key,		/* in/out: key to search for (or found if blank) */
char **value,		/* out: pointer to value found */
int *vallen		/* out: length of value found */
)

{
   char *ploc, *ip;

   ploc = v2_find_entry(*start, key, value, vallen, &ip);

   if ((ploc == NULL) || (ploc > *stop)) {
      *start = *stop;		/* Not found, so set the pointers the same */
      return NO_SUCH_KEY;
   }

   *start = ip;
   *stop = ploc;

   return SUCCESS;
}
