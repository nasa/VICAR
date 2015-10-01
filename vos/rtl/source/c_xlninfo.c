#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xlninfo(int unit, char *key, char *format, int *maxlength, int *nelement)
{
   int status;
   int level, strlength;
   char *p, *dummy;
   char *value;
   int vallen;

   if (SEQ_DEVICE && (CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN))
      return TOO_LATE;

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

   p = LABEL_S_VALUE(POSITION);		/* Get last position in label */
   if (p == NULL)			/* if NULL, set it to the beginning */
      p = CURRENT_S_VALUE(LABELS);

   *key = '\0';		/* Null the key so we get the next item */
   p = v2_find_entry(p, key, &value, &vallen, &dummy);
   if (p == NULL)
      return END_OF_LABEL;

   *format = '\0';		/* make null for evaluate_label_item */
   status = v2_evaluate_label_item(value, vallen, format, maxlength,
		nelement, &level, &strlength);
   if (status != SUCCESS)
      return status;

   if (LABEL_IP_VALUE(STRLEN) != 0)
      *LABEL_IP_VALUE(STRLEN) = strlength;

   LABEL_S_VALUE(POSITION) = p;		/* save for next call */

   return SUCCESS;
}
