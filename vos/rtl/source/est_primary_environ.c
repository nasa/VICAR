#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Save away values from the primary input file for later */

int v2_est_primary_environ(int unit)
{
   int i, status;
   char *p = NULL;
   struct bufstate *primary_bufstate;

   /* Copy the value table created when the primary was opened	*/
   /* to a special primary_input table.				*/

   for (i=0; i < N_UNIT_TABLE_ENTRIES; i++) {
      if (i==IO_ACT)
         continue;
      if (i==CLOS_ACT)
         continue;
      if (i==IO_MESS)
         continue;
      v2_copy_value_block_item(unit, primary_input_unit, i);
   }

   /* If labels are present then read the primary labels into	*/
   /* memory to support later label processing.			*/

   if (!(CURRENT_I_VALUE(FLAGS) & NO_LABELS)) {
      status = v2_read_in_labels(primary_input_unit,&p,
					&PRIMARY_I_VALUE(LBLALLOC));
      if (status != SUCCESS)
         return status;
   }

   primary_allocation_size = 0;	       /* For the benefit of open_disk_output */
   primary_bufstate = (struct bufstate *)PRIMARY_IP_VALUE(BUFSTATE);
#if VMS_OS
   if (primary_bufstate->devstate.device_type == DEV_DISK ||
       primary_bufstate->devstate.device_type == DEV_ARRAY)
      primary_allocation_size = primary_bufstate->devstate.dev.disk.allocation;
#endif
   primary_eof_record = (int)primary_bufstate->eof_record;

/* Note:  The loop above copies the BUFSTATE pointer to the primary input.  */
/* It is used for read_in_labels, and to get the allocation size, but now   */
/* it's not needed any more so we null the  pointer.  This is so we don't   */
/* have two pointers (unit & primary unit) to the same bufstate, which      */
/* would cause problems when the bufstate was deallocated.		    */

   PRIMARY_IP_VALUE(BUFSTATE) = NULL;

   PRIMARY_S_VALUE(LABELS) = p;		/* Save the location of the labels. */

   return SUCCESS;
}
