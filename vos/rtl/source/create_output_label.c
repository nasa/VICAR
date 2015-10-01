#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Create the label for a new output file */

int v2_create_output_label(int unit)
{
   int status;
   int lblsize;
   char *recbuf;
   char buf[MAX_SIMPLE_LABEL_ITEM_SIZE+1];

   label_record = 0;

   recbuf = malloc(CURRENT_I_VALUE(RECSIZE)+1);	/* +1 for null term */
   if (recbuf==NULL)
     return INSUFFICIENT_MEMORY;

   *recbuf = '\0';			/* Make sure it's a c string. */

   memset(buf, 0, MAX_SIMPLE_LABEL_ITEM_SIZE+1);
   v2_add_label_size_item(buf, 0);		/* Init labelsize item to 0 */

   status = v2_move_to_output_label(unit, buf, strlen(buf), recbuf);
   if (status != SUCCESS) {
      free(recbuf);
      return status;
   }

   /* Maintain a count of the label size as the output label is	*/
   /* built so that the LBLSIZE item in the system label can be	*/
   /* added.							*/

   lblsize = strlen(buf);

   status = v2_build_system_label(unit, recbuf, &lblsize);
   if (status != SUCCESS) {
      free(recbuf);
      return status;
   }

   /* Property labels */

   if (primary_input_open && !(PRIMARY_I_VALUE(FLAGS) & NO_LABELS)) {
      status = v2_trans_prim_prop_labels(unit, &lblsize, recbuf);
      if (status != SUCCESS) {
         free(recbuf);
         return status;
      }
   }

   /* History labels */

   if (primary_input_open && !(PRIMARY_I_VALUE(FLAGS) & NO_LABELS)) {
      status = v2_trans_prim_hist_labels(unit, &lblsize, recbuf);
      if (status != SUCCESS) {
         free(recbuf);
         return status;
      }
   }

   status = v2_trans_curr_hist_item(unit, &lblsize, recbuf);
   if (status != SUCCESS) {		/* Also flushes out 'recbuf' buffer */
      free(recbuf);
      return status;
   }
      
   status = v2_update_label_size_item(unit, lblsize);
   if (status != SUCCESS) {
      free(recbuf);
      return status;
   }

   free(recbuf);

   return SUCCESS;		/* The label has now been written */
}
