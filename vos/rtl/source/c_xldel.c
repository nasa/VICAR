#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xldel(int unit, char *type, char *key)
{
   int status;
   int start_element, nelements;
   char *value;
   int vallen;
   char *p, *newstop;
   char *start, *stop;

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN))
      return FILE_NOT_OPEN;

   if (SEQ_DEVICE && (CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN))
      return TOO_LATE;

   if (EQUAL(CURRENT_S_VALUE(OP), "READ"))
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
   /* a property in the property label, or the system label.  "Start"	*/
   /* and "stop" end up pointing at the	label substring containing the	*/
   /* desired portion.							*/

   if (EQUAL(type, "HISTORY")) {		/* History label */
      status = v2_find_task(unit, LABEL_S_VALUE(HIST), LABEL_I_VALUE(INSTANCE),
			 &start, &stop);
      if (status != SUCCESS)
         return status;
   }
   else if (EQUAL(type, "PROPERTY")) {		/* Property label */
      if (strlen(LABEL_S_VALUE(PROPERTY)) == 0)	/* Must have a property name */
         return PROPERTY_OPTIONAL_REQUIRED;
      status = v2_find_property(unit, LABEL_S_VALUE(PROPERTY),
			LABEL_I_VALUE(INSTANCE), &start, &stop, NULL);
      if (status != SUCCESS)
         return status;
   }
   else if (EQUAL(type, "SYSTEM")) {		/* System label */
      status = v2_find_system(unit, &start, &stop);
      if (status != SUCCESS)
         return status;
   }
   else return BAD_LABEL_TYPE;	/* Neither HISTORY nor SYSTEM nor PROPERTY */

   status = v2_find_key(&start, &stop, key, &value, &vallen);
   if (status != SUCCESS)
      return CANNOT_FIND_KEY;

   start_element = LABEL_I_VALUE(ELEMENT)-1;
   if (LABEL_I_VALUE(ELEMENT) == 0)		/* defaulted */
      start_element = 0;
   if (LABEL_I_VALUE(ELEMENT) == -1)		/* special value */
      start_element = -1;

   nelements = LABEL_I_VALUE(NELEMENTS);
   if (LABEL_I_VALUE(NELEMENTS) == 0)		/* defaulted */
      nelements = -1;				/* all the elements */

   status = v2_delete_label_item(unit, start, stop, key, value, vallen,
		start_element, &nelements, &newstop);
   if (status != SUCCESS)
      return status;

   if (LABEL_IP_VALUE(NRET) != 0)	/* Return optionals */
      *LABEL_IP_VALUE(NRET) = nelements;

   CURRENT_I_VALUE(FLAGS) |= LABELS_MODIFIED;

   LABEL_S_VALUE(POSITION) = newstop;	/* for xlninfo */

   return SUCCESS;
}
