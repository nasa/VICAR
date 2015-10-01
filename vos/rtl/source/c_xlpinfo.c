#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xlpinfo(
   int unit,
   char *props,
   int *nprop,			/* in: size of array; out: tot # of props */
   int len			/* length for props array (0 is not valid) */
)

{
   int status, i;
   int labno, max_nprop, length;
   char *p, *t, *dummy;
   char *value;
   int vallen;
   int *instances;

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

   p = CURRENT_S_VALUE(LABELS);

   if (*nprop <= 0)
      return NULL_REQUEST;

   /* Scan through the label looking for the property items. */

   labno = 0;
   max_nprop = *nprop;
   length = len;
   if (len == 0)
      return IMPROPER_LENGTH;

   *nprop = 0;

   /* too bad "instances" can't be a parameter... (for historical reasons) */
   instances = LABEL_IP_VALUE(INST_NUM);

   while ((p = v2_find_entry(p, PROPERTY_KEY, &value, &vallen, &dummy))) {
      if (labno < max_nprop) {	/* there's room */
         t = v2_dequoted(value,vallen);
         strncpy(props+(labno*length), t, length);
         *(props+(labno*length)+length-1) = '\0'; /* make sure null present*/

         if (instances) {	/* do instance check only if requested */
            instances[labno] = 1;
            for (i = labno-1; i >= 0; i--) { /* find the inst for this prop */
               if (strcmp(props+(i*length), t) == 0) {
                  instances[labno] = instances[i] + 1;
                  break;
               }
            }
         }
         *nprop = labno+1;
      }
      else {			/* out of room in user buffer */
         if (LABEL_IP_VALUE(NRET) == NULL)
            break;	/* stop if they don't want the total # of props */
      }
      labno++;
   }

   if (LABEL_IP_VALUE(NRET) != NULL)
      *LABEL_IP_VALUE(NRET) = labno;

   return SUCCESS;
}
