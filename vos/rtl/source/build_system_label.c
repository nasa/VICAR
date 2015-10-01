#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Creates the system label from the unit table.  See comments	*/
/* for move_to_output_label() for explanation of 'recbuf'.	*/
/* *lblsize is incremented by the length of label added.	*/

int v2_build_system_label(
   int unit,
   char *recbuf,
   int *lblsize		/* in/out:  size of label (incremented here) */
)

{
   int i, status;
   char ps[MAX_STRING_SIZE*2+2];

   for (i=0; i<N_UNIT_TABLE_ENTRIES; i++)
   {
      if (unit_table[i].mode != SYSTEM)		/* Only include SYSTEM items */
         continue;
      if (i == LBLSIZE)
         continue;		/* Add this later when size known */

      if (unit_table[i].type == INTEGER)
         sprintf(ps, "%s=%d  ", unit_table[i].name, CURRENT_I_VALUE(i));

      else if (unit_table[i].type == STRING)
         sprintf(ps, "%s='%s'  ", unit_table[i].name, CURRENT_S_VALUE(i));

      status = v2_move_to_output_label(unit, ps, strlen(ps), recbuf);
      if (status != SUCCESS)
         return status;

      *lblsize += strlen(ps);
   }

   return SUCCESS;
}
