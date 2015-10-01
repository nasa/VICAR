#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xladd(
   int unit,
   char *type,
   char *key,
   void *newvalue,
   int len			/* len of C array for strings */
)

{
   int status;
   int level, nelements, element;
   char *p, *dummy;
   char *start, *stop;
   char *value = NULL;
   int vallen;
   int inst_out;

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN))
      return FILE_NOT_OPEN;

   if (SEQ_DEVICE && (CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN))
      return TOO_LATE;

   if (EQUAL(CURRENT_S_VALUE(OP),"READ"))
      return CAN_NOT_MODIFY_AN_INPUT_LABEL;

   if (CURRENT_I_VALUE(FLAGS) & NO_LABELS)
      return FILE_HAS_NO_LABEL;

   /* Before label processing occurs, the labels must be read into local mem */

   if (CURRENT_S_VALUE(LABELS) == NULL) {
      status = v2_read_in_labels(unit, &p, &CURRENT_I_VALUE(LBLALLOC));
      if (status != SUCCESS)
         return status;
      CURRENT_S_VALUE(LABELS) = p;
   }

   /* Find desired label portion... either a task in the history label,	*/
   /* a property, or the system label.  "Start" and "stop" end up	*/
   /* pointing at the label substring containing the desired portion.	*/

   if (EQUAL(type, "HISTORY")) {		/* History label */
      status = v2_find_task(unit, LABEL_S_VALUE(HIST), LABEL_I_VALUE(INSTANCE),
			 &start, &stop);
      if (status != SUCCESS)
         return status;
   }
   else if (EQUAL(type, "SYSTEM")) {		/* System label */
      status = v2_find_system(unit, &start, &stop);
      if (status != SUCCESS)
         return status;
   }
   else if (EQUAL(type, "PROPERTY")) {		/* Property label */
      if (strlen(LABEL_S_VALUE(PROPERTY)) == 0)	/* Must have a property name */
         return PROPERTY_OPTIONAL_REQUIRED;
      status = v2_find_property(unit, LABEL_S_VALUE(PROPERTY),
			LABEL_I_VALUE(INSTANCE), &start, &stop, &inst_out);

      if (status != SUCCESS) {	/* Property doesn't exist, so create it */

	 /* New property label goes just before history */

         LABEL_I_VALUE(INSTANCE) = inst_out + 1;	/* in case 0 for inst */

         v2_find_first_hist_item(CURRENT_S_VALUE(LABELS), &p);
         status = v2_add_label_item(unit, LABEL_S_VALUE(PROPERTY), p, p,
			PROPERTY_KEY, NULL, 0, 1, 1, 0, "STRING", 0);
         if (status != SUCCESS)
            return status;

         /* Now find it again to set up pointers */

         status=v2_find_property(unit, LABEL_S_VALUE(PROPERTY),
			LABEL_I_VALUE(INSTANCE), &start, &stop, NULL);
         if (status != SUCCESS)		/* whoops! */
            return status;
      }
   }
   else return BAD_LABEL_TYPE;	/* Neither HISTORY nor SYSTEM nor PROPERTY */

   if (strlen(LABEL_S_VALUE(LFORMAT)) == 0)	/* this is relied on in the */
      return FORMAT_OPTIONAL_REQUIRED;		/* v2_add_label_item call */

   level = LABEL_I_VALUE(LEVEL);
   nelements = LABEL_I_VALUE(NELEMENTS);
   if (LABEL_I_VALUE(NELEMENTS) == 0)	/* defaulted */
      nelements = 1;
   if (LABEL_I_VALUE(NELEMENTS) == -1)		/* what does 'all elements' */
      return IMPROPER_ELEMENT_NUMBER;		/* mean if you're adding?   */

   element = LABEL_I_VALUE(ELEMENT) - 1;
   if (LABEL_I_VALUE(ELEMENT) == 0)		/* defaulted */
      element = -1;				/* put it at end */
   if (LABEL_I_VALUE(ELEMENT) == -1)
      element = -1;				/* put it at end */

   /* Make sure the key does not already exist. */

   status = v2_find_key(&start, &stop, key, &value, &vallen);
   if (status==SUCCESS) {			/* it already exists */
      if (EQUAL(LABEL_S_VALUE(MODE), "ADD"))
         return DUPLICATE_KEY;
      if (EQUAL(LABEL_S_VALUE(MODE), "REPLACE")) {
         if (LABEL_I_VALUE(NELEMENTS) == 0)	/* defaulted */
            nelements = -1;
         status = v2_delete_label_item(unit, start, stop, key, value, vallen,
			element, &nelements, &dummy); /* del items to replace */
         if (status != SUCCESS)
            return status;

         nelements = LABEL_I_VALUE(NELEMENTS);	/* restore what del clobbered */
         if (LABEL_I_VALUE(NELEMENTS) == 0)		/* defaulted */
            nelements = 1;
         element = LABEL_I_VALUE(ELEMENT) - 1;
         if (LABEL_I_VALUE(ELEMENT) == 0)		/* defaulted */
            element = -1;			/* put it at end */
         if (LABEL_I_VALUE(ELEMENT) == -1)
            element = -1;			/* put it at end */

	/* We must call v2_find_task and v2_find_key again because all the */
	/* pointers could have changed as a result of the delete.          */

         if (EQUAL(type, "HISTORY")) {		/* History label */
            status = v2_find_task(unit, LABEL_S_VALUE(HIST),
				LABEL_I_VALUE(INSTANCE), &start, &stop);
            if (status != SUCCESS)
               return status;
         }
         else if (EQUAL(type, "PROPERTY")) {	/* Property label */
            status = v2_find_property(unit, LABEL_S_VALUE(PROPERTY),
				LABEL_I_VALUE(INSTANCE), &start, &stop, NULL);
            if (status != SUCCESS)
               return status;
         }
         else {					/* must be System */
            status = v2_find_system(unit, &start, &stop);
            if (status != SUCCESS)
	       return status;
         }
         v2_find_key(&start, &stop, key, &value, &vallen);
      }	   /* MODE=INSERT needs no special processing if label already exists */
   }

   /* Add the item to the label */

   status = v2_add_label_item(unit, newvalue, start, stop, key, value, vallen,
		element, nelements, level, LABEL_S_VALUE(LFORMAT), len);
   if (status != SUCCESS)
      return status;

   CURRENT_I_VALUE(FLAGS) |= LABELS_MODIFIED;

   /* Now, re-find the item in the modified label so that	*/
   /* POSITION may be updated.					*/

   if (EQUAL(type,"HISTORY")) {		/* History label */
      status = v2_find_task(unit, LABEL_S_VALUE(HIST), LABEL_I_VALUE(INSTANCE),
			 &start, &stop);
      if (status != SUCCESS)
         return status;
   }
   else if (EQUAL(type, "PROPERTY")) {	/* Property label */
      status = v2_find_property(unit, LABEL_S_VALUE(PROPERTY),
			LABEL_I_VALUE(INSTANCE), &start, &stop, NULL);
      if (status != SUCCESS)
         return status;
   }
   else {				/* Must be System */
      status = v2_find_system(unit, &start, &stop);
      if (status != SUCCESS)
         return status;
   }

   status = v2_find_key(&start, &stop, key, &value, &vallen); /* find key */
   if (status != SUCCESS)
      return status;

   /* POSITION is the place after the found item in the label.  XLNINFO	*/
   /* uses POSITION to find it's 'next' item.				*/

   LABEL_S_VALUE(POSITION) = stop;

   /* If this is not a SYSTEM label, then we're done */

   if (!EQUAL(type, "SYSTEM"))
      return SUCCESS;

   /* The keys NL, NB, and NLB are special and require updates	*/
   /* to the current table.  The following code takes care of	*/
   /* these updates.  Note:  ORG used to be handled here, but	*/
   /* not any more.  You can't change ORG's in mid-stream,	*/
   /* because it could very well change the record size.	*/

   if (EQUAL(key,"NL"))
      CURRENT_I_VALUE(NL) = *(int *)newvalue;
   else if (EQUAL(key,"NB"))
      CURRENT_I_VALUE(NB) = *(int *)newvalue;
   else if (EQUAL(key,"NLB"))
      CURRENT_I_VALUE(NLB) = *(int *)newvalue;
   else			/* Not a special system label, so we're done */
      return SUCCESS;

   /* The table has been updated, so reconcile NL, NS,	*/
   /* and NB with N1, N2, and N3.			*/

   if (EQUAL(CURRENT_S_VALUE(ORG),"BSQ")) {
      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NL);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NB);
   }
   else if (EQUAL(CURRENT_S_VALUE(ORG),"BIL")) {
      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NB);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NL);
   }
   else if (EQUAL(CURRENT_S_VALUE(ORG),"BIP")) {
      CURRENT_I_VALUE(N1) = CURRENT_I_VALUE(NB);
      CURRENT_I_VALUE(N2) = CURRENT_I_VALUE(NS);
      CURRENT_I_VALUE(N3) = CURRENT_I_VALUE(NL);
   }
   else
      return BAD_ORG;

   return SUCCESS;
}
