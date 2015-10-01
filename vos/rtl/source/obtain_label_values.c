#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Get all the element values from a label into an array (of	*/
/* type indicated by format).					*/

int v2_obtain_label_values(
   char *value,		/* in: ptr to value of item */
   int vallen,		/* in: length of value */
   char *valarray,	/* out: array of element values (alloced by caller) */
   int nelements,	/* in: number of elements in item */
   int maxlength,	/* in: max length of items (and inner dim of valarray)*/
   char *format		/* in: format of items */
)

{
   int status;
   int level = 0;
   char *element;
   char *converted_item;
   char *i, *j;

   memset(valarray, 0, maxlength * nelements);

   /* If item is binary, just remove brackets and return it (NOT IMPLEMENTED) */
   if (EQUAL(format,"BIN")) {
      i = strchr(value,'{');
      j = strrchr(value,'}');
      strncpy(valarray,value,j-i+1);
      return SUCCESS;
   }

   while (v2_get_label_item(&value, &vallen, &element, &level)) {
      status = v2_convert_from_text(&converted_item, element, maxlength,format);
      if (status != SUCCESS)
         return status;

      v2_move(valarray, converted_item, maxlength);  /* copy elem to valarray */

      valarray += maxlength;	/* jump to next element address */
   }

   return SUCCESS;
}
