#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xlinfo(int unit, char *type, char *key, char *format,
		int *maxlength, int *nelement)
{
   int status;
   int level, strlength;
   char *start, *stop, *p;
   char *value;
   int vallen;

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN))
      return FILE_NOT_OPEN;

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
   /* or the system label.  "Start" and "stop" end up pointing at the	*/
   /* label substring containing the desired portion.			*/

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

   *format = '\0';		/* make null for evaluate_label_item */
   status = v2_evaluate_label_item(value, vallen, format, maxlength,
		nelement, &level, &strlength);
   if (status != SUCCESS)
      return status;

   if (LABEL_IP_VALUE(STRLEN) != 0)
      *LABEL_IP_VALUE(STRLEN) = strlength;

   LABEL_S_VALUE(POSITION) = stop;	/* save for xlninfo */

   return SUCCESS;
}
