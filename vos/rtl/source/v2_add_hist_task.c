#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Adds a history label to the end of the label list of unit.  This returns */
/* SUCCESS unless there is a problem with reading or inserting the label.   */
/* This routine will only be used in UPDATE mode, not for output mode since */
/* there is a different method used for writing labels for output mode.     */
int v2_add_hist_task(int unit)
{
   char *p,*position;
   char label_item[MAX_STRING_SIZE+1];
   int status;

   /* Before label processing occurs, the labels must be read */
   /* into local mem */

   if (CURRENT_S_VALUE(LABELS) == NULL) {
      status = v2_read_in_labels(unit, &p, &CURRENT_I_VALUE(LBLALLOC));
      if (status != SUCCESS)
         return status;
      CURRENT_S_VALUE(LABELS) = p;
   }

   /* Add history label to current unit */

   label_item[0] = '\0';

   v2_build_history_label(label_item);

   position = p + strlen(p);

   status = v2_insert_label_string(unit, position, label_item);
   if (status != SUCCESS)
      return status;

   return SUCCESS;
}
