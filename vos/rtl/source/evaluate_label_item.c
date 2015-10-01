#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Given a label value, this routine returns the number of elements	*/
/* and the max length of any element.  If strlen(format)==0, then a	*/
/* guess as to the format of the item is made and returned in format.	*/
/* Strlength gets the length of the item treated as if it were a string.*/
/* All elements are assumed to have the same format as the first.	*/

int v2_evaluate_label_item(
   char *value,			/* in: pointer to value */
   int vallen,			/* in: length of value */
   char *format,		/* in/out: format of value (guessed fmt if 0) */
   int *length,			/* out: max length of any element */
   int *nelements,		/* out: # of elements */
   int *level,			/* out: level of the value (not done right) */
   int *strlength		/* out: length of item treated as a string */
)

{
   int maxlen;
   char *element;

   *nelements = 0;
   *level = 0;
   maxlen = 0;

   /* Get each element value until no more remain */

   while (v2_get_label_item(&value, &vallen, &element, level)) {
      (*nelements)++;
      maxlen = MAX(maxlen, (int) strlen(v2_dequoted(element,strlen(element))));
      if (*nelements == 1) {		/* if first element, get the format */
         if (strlen(format) == 0)
            strcpy(format, v2_determine_format(element));
      }
   }

   *strlength = maxlen;

   if (EQUAL(format, "INT"))
      *length = sizeof(int);
   else if (EQUAL(format, "REAL"))
      *length = sizeof(float);
   else if (EQUAL(format, "DOUB"))
      *length = sizeof(double);
   else
      *length = maxlen;

   return SUCCESS;
}
