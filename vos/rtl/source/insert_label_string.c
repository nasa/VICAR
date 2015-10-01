#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Inserts the string 'newstr' into a string at 'loc'.  If there's not	*/
/* enough room left in the current label allocation, then the label is	*/
/* re-allocated and copied.  Note that this invalidates all pointers	*/
/* into the label area.							*/

int v2_insert_label_string(
   int unit,		/* in: unit number */
   char *loc,		/* in: place in label to insert at */
   char *newstr		/* in: string to insert */
)

{
   int newlen, size, remaining;
   int offset;
   char *labels;

   newlen = strlen(newstr);

   /* Re-allocate the labels if there's not enough room */

   if (newlen + (int) strlen(CURRENT_S_VALUE(LABELS)) >= CURRENT_I_VALUE(LBLALLOC)) {

      offset = loc - CURRENT_S_VALUE(LABELS); /* convert pointer to an offset */

      size = CURRENT_I_VALUE(LBLALLOC) + newlen + LABEL_BUFFER_EXTRA;
      labels = malloc(size);
      if (labels == NULL)
         return NO_MEMORY_FOR_LABEL_PROCESS;

      strcpy(labels, CURRENT_S_VALUE(LABELS));	/* copy the labels over */
      CURRENT_I_VALUE(LBLALLOC) = size;

      free(CURRENT_S_VALUE(LABELS));
      CURRENT_S_VALUE(LABELS) = labels;

      loc = offset + CURRENT_S_VALUE(LABELS); /* convert offset back to ptr */
   }

   remaining = strlen(loc);

   v2_move(loc + newlen, loc, remaining+1); /* make hole (+1 incl terminator) */
   v2_move(loc, newstr, newlen);		   /* stick the new string in */

   return SUCCESS;
}
