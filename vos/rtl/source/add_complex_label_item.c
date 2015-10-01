#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* A complex label item is another name for a multi-valued label item.	*/
/* This routine adds elements to an existing multi-valued label item.	*/
/* The various label pointers (start, stop, value, etc.) may not be	*/
/* valid after this routine finishes if the label was re-allocated.	*/

int v2_add_complex_label_item(
   int unit,		/* in: unit number of file */
   char *newvalue,	/* in: new value vector */
   char *value,		/* in: current item's value pointer */
   int vallen,		/* in: length of value */
   char *start,		/* in: starting point of current label */
   char *stop,		/* in: ending point of current label */
   int element,		/* in: starting element # to add */
   int nelements_to_add, /* in: # of elements to add (and 2nd dim of newvalue)*/
   int newlength,	/* in: inner dimension of newvalue */
   int level,		/* in: level number (NOT IMPLEMENTED) */
   char *format,	/* in: format of item */
   char *key		/* in: key of item */
)

{
   int i, status;
   int length, oldlength;
   int nelements, nelements_present;
   char *valarray;
   int dummy;
   char *label_item;

   status = v2_evaluate_label_item(value, vallen, format, &oldlength,
				&nelements_present, &level, &dummy);
   if (status != SUCCESS)
      return status;

   if (EQUAL(format, "STRING"))
      oldlength++;		/* leave room for null terminator */

   length = MAX(newlength, oldlength);

   if (element == -1 || element > nelements_present)	/* add at end */
      element = nelements_present;

   if (nelements_to_add <= 0)
      return SUCCESS;				/* nothing to add */

   nelements = nelements_present + nelements_to_add;

   valarray = malloc(length * nelements + 1);
   if (valarray==NULL)
      return NO_MEMORY_FOR_LABEL_PROCESS;

   status = v2_obtain_label_values(value, vallen, valarray, nelements_present,
				length, format);
   if (status != SUCCESS) {
      free(valarray);
      return status;
   }

   if (element < nelements_present)	/* make room for new elements */
      v2_move(valarray + (element + nelements_to_add) * length,
	     valarray + element * length,
	     (nelements_present - element) * length);

   if (length == newlength)	     /* the arrays match, so use simple move */
      v2_move(valarray + element * length,
	     newvalue,
	     nelements_to_add * length);

   else {  /* arrays don't match in size, so move each element independently */

      for (i=0; i<nelements_to_add; i++)
         v2_move(valarray + (element + i) * length,
		newvalue + i * newlength,
		length);
   }

   /* Create new label item with the new values */

   status = v2_create_label_item(key, valarray, length, nelements, format,
			      level, &label_item);
   if (status != SUCCESS) {
      free(valarray);
      return status;
   }

   /* Cut out old label item between 'start' and 'stop' and insert new one */

   status = v2_cut_label_string(start, stop, "");	/* remove old item */
   if (status != SUCCESS) {
      free(valarray);
      free(label_item);
      return status;
   }

   status = v2_insert_label_string(unit, start, label_item); /* insert new one*/

   free(valarray);
   free(label_item);
   return status;
}
