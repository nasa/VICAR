#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Delete a substring of the label.  If the key is multi-valued, you	*/
/* can delete only a portion of it with start_element and nelements.	*/

int v2_delete_label_item(
   int unit,		/* in: unit number */
   char *start,		/* in: pointer to beginning of label to delete */
   char *stop,		/* in: pointer past end of label to delete */
   char *key,		/* in: key of the item to delete */
   char *value,		/* in: pointer to the value of the item to delete */
   int vallen,		/* in: length of the value */
   int start_element,	/* in: starting element to delete for multi-valued */
   int *nelements,	/* in/out: # elems to delete (-1==all) (out: # del'd) */
   char **newstop	/* out: ptr to new end of label item (updated stop) */
)

{
   int status;
   int length, ielements, level;
   int dummy;
   char format[MAX_SHORT_STRING_SIZE+1];

   *newstop = stop;

   strcpy(format, LABEL_S_VALUE(LFORMAT));

   status = v2_evaluate_label_item(value, vallen, format, &length, &ielements,
				&level, &dummy);	/* get ielements */
   if (status != SUCCESS)
      return status;

   if (EQUAL(format, "STRING"))
      length++;			/* leave room for null terminator */

   if (*nelements == -1)		/* delete all elements */
      *nelements = ielements;
   if (start_element == -1 || start_element >= ielements) {
      *nelements = 0;
      return SUCCESS;			/* nothing to delete */
   }

   /* If label item is single valued, or entire item is to be deleted,	*/
   /* then cut the item out of the label.				*/

   if ((ielements == 1) || ((*nelements >= ielements) && (start_element ==0))) {
      *nelements = ielements;
      v2_cut_label_string(start, stop, "");
      *newstop = start;
   }
   else {	/* else remove only those values of the label item desired */
      status = v2_del_complex_label_item(value, vallen, start, stop,
			start_element, nelements, ielements, length, level,
			format, key, newstop);
      if (status != SUCCESS)
         return status;
   }

   return SUCCESS;
}
