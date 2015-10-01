#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xlhinfo(
   int unit,
   char *tasks,
   int *instances,
   int *nhist,			/* in: size of array; out: tot # of tasks */
   int len			/* length for tasks array (if 0, use 8) */
)
{
   int status, i;
   int labno, max_nhist, length;
   char *p, *t, *dummy;
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

   p = CURRENT_S_VALUE(LABELS);

   if (*nhist <= 0)
      return NULL_REQUEST;

   /* Scan through the label looking for the history items. */

   labno = 0;
   max_nhist = *nhist;
   length = len;
   if (len == 0)
      length = MAX_HIST_NAME_SIZE;

   *nhist = 0;

   while ((p = v2_find_entry(p, TASK_KEY, &value, &vallen, &dummy))) {
      if (labno < max_nhist) {	/* there's room */
         t = v2_dequoted(value,vallen);
         strncpy(tasks+(labno*length), t, length);
         if (len != 0)		/* not special 8-byte, no terminator format */
            *(tasks+(labno*length)+length-1) = '\0'; /* make sure null present*/

         instances[labno] = 1;
         for (i = labno-1; i >= 0; i--) { /* find the instance for this task */
            if (strncmp(tasks+(i*length), t, length) == 0) {
               instances[labno] = instances[i] + 1;
               break;
            }
         }
         *nhist = labno+1;
      }
      else {			/* out of room in user buffer */
         if (LABEL_IP_VALUE(NRET) == NULL)
            break;	/* stop if they don't want the total # of tasks */
      }
      labno++;
   }

   if (LABEL_IP_VALUE(NRET) != NULL)
      *LABEL_IP_VALUE(NRET) = labno;

   return SUCCESS;
}
