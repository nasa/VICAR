#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* If start==stop, then the label item doesn't currently exist, so add a */
/* label item of the form 'key=value' into the label string at 'stop'.   */
/* If start!=stop, then the label item exists, so insert the elements    */
/* into the existing label item.  The pointers involved (start, stop,	 */
/* value, etc.) may not be valid on exit from this routine, if the label */
/* has been re-allocated.						 */

int v2_add_label_item(
   int unit,		/* in: unit number of file */
   char *newvalue,	/* in: new value vector to use */
   char *start,		/* in: start of current label item */
   char *stop,		/* in: end of current label item */
   char *key,		/* in: key of new item */
   char *value,		/* in: pointer to value of current item */
   int vallen,		/* in: length of value */
   int element,		/* in: element # to start adding at */
   int nelements,	/* in: # of elements to add (also size of newvalue) */
   int level,		/* in: level number (NOT IMPLEMENTED) */
   char *format,	/* in: format of item */
   int maxlen		/* in: max length of items (inner dim of newvalue) */
)

{
   int status;
   char *label_item;

   if (maxlen == 0) {		/* if still not present, try to figure it out */
      if (EQUAL(format, "STRING"))
         if (nelements == 1)
            maxlen = strlen(newvalue) + 1;
         else
            return IMPROPER_LENGTH;
      else if (EQUAL(format, "INT"))
         maxlen = sizeof(int);
      else if (EQUAL(format, "REAL"))
         maxlen = sizeof(float);
      else if (EQUAL(format, "DOUB"))
         maxlen = sizeof(double);
      else
         return ILLEGAL_FORMAT_REQUEST;
   }

   if (start != stop) {			/* adding to existing label */
      status = v2_add_complex_label_item(unit, newvalue,value,vallen,start,stop,
			element, nelements, maxlen, level, format, key);
      if (status != SUCCESS)
         return status;
   }
   else {
      status = v2_create_label_item(key, newvalue, maxlen, nelements,
			format, level, &label_item);
      if (status != SUCCESS)
         return status;

      strcat(label_item,"  ");		/* leave space between labels */

      status = v2_insert_label_string(unit, stop, label_item);
      free(label_item);
      if (status != SUCCESS)
         return status;
   }

   return SUCCESS;
}
