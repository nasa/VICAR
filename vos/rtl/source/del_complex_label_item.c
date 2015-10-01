#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* A complex label item is another name for a multi-valued label item.	*/
/* This routine deletes a subset of the item's elements and leaves the	*/
/* rest of the elements in place.					*/

int v2_del_complex_label_item(
   char *value,		/* in: pointer to the item's value */
   int vallen,		/* in: length of value */
   char *start,		/* in: starting point of label */
   char *stop,		/* in: ending point of label */
   int start_element,	/* in: starting element to delete */
   int *nelements_to_delete,/*in/out: # of elements to delete (out: # deleted)*/
   int nelements_present, /* in: number of elements present in current item */
   int length,		/* in: maxlength from evaluate_label_item */
   int level,		/* in: level (not implemented) */
   char *format,	/* in: format of item */
   char *key,		/* in: key of item */
   char **newstop	/* out: pointer to new stop location of label */
)

{
   int status, n;
   char *valarray;
   char *label_item;

   *newstop = stop;

   /* Allocate storage for the elements of the item */

   valarray = malloc(length * nelements_present + 1);
   if (valarray==NULL)
      return NO_MEMORY_FOR_LABEL_PROCESS;

   status = v2_obtain_label_values(value, vallen, valarray, nelements_present,
				length, format);
   if (status != SUCCESS) {
      free(valarray);
      return status;
   }

   if ((start_element<0) || (start_element>=nelements_present)) {
      free(valarray);
      return SUCCESS;			/* nothing to delete */
   }

   if (start_element + *nelements_to_delete > nelements_present)
      *nelements_to_delete = nelements_present - start_element;

   if (*nelements_to_delete <= 0) {
      free(valarray);
      return SUCCESS;			/* nothing to delete */
   }

   /* Move all elements after the deleted section down to fill	*/
   /* in.  The # of elements to move is given by n.		*/

   n = nelements_present - start_element - *nelements_to_delete;

   v2_move(valarray + (start_element * length),
	  valarray + ((start_element + *nelements_to_delete) * length),
	  n * length);

   status = v2_create_label_item(key, valarray, length, start_element+n, format,
		level, &label_item);
   if (status != SUCCESS) {
      free(valarray);
      return status;
   }

   /* Cut out the old label (between start and stop), and insert the	*/
   /* newly created one in its place.					*/

   status = v2_cut_label_string(start, stop, label_item);
   if (status != SUCCESS) {
      free(valarray);
      free(label_item);
      return status;
   }

   *newstop = start + strlen(label_item);

   free(valarray);
   free(label_item);
   return SUCCESS;
}
