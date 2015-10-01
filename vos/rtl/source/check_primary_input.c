#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Label information from the primary input is used in the allocation	*/
/* and labeling of output files. This routine determines if the input	*/
/* unit is the primary input and if so establishes the primary input	*/
/* environment.								*/

int v2_check_primary_input(int unit)
{
   int status;

   /* If the name associated with the unit is not "INP" or if it is but	*/
   /* not the correct instance in a multi-file list then return... if	*/
   /* we're instance based.						*/

   if (primary_instance >= 0) {
      if (! EQUAL(active_units[unit].name, "INP") ||
          (active_units[unit].instance != primary_instance))
         return SUCCESS;
   }
   else {	/* < 0 means use primary_unit_requested instead of instance */
      if (unit != primary_unit_requested)
         return SUCCESS;
   }

   /* If the primary input is already open, close it and re-open it to	*/
   /* get any changes.							*/

   if (primary_input_open) {
      if (PRIMARY_S_VALUE(LABELS) != 0)
         free(PRIMARY_S_VALUE(LABELS));
      v2_close_unit(primary_input_unit);
      primary_input_open = FALSE;	/* in case activate_a_unit fails */
   }

   status = v2_activate_a_unit(&primary_input_unit, 1, PRIMARY_NAME);
   if (status != SUCCESS)
      return status;

   status = v2_est_primary_environ(unit);
   if (status != SUCCESS) {
      v2_close_unit(primary_input_unit);
      return status;
   }
   primary_input_open = TRUE;
   PRIMARY_I_VALUE(FLAGS) &= ~OPEN;	/* the file is not open via this unit */

   return SUCCESS;
}
