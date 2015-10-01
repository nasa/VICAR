#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* If primary input file is not yet open, then open it to set	*/
/* the various values, and close it again.  If it is already	*/
/* open, then just copy the values to the primary unit.		*/

int v2_est_primary_input(void)
{
   int status;
   int unit;
   char *name;

   if (!primary_input_open) {

      if (primary_instance == 0)
         return BAD_INPUT_LABEL;	/* so caller will continue w/o PI */

      /* Use INP instance, or unit number? */

      if (primary_instance > 0) {	/* INP instance */
         status = v2_get_one_string_parm("INP", primary_instance-1, &name);
         if (status == SUCCESS && *name != '\0') {

            status = zvunit(&unit, "INP", primary_instance, NULL);
            if (status != SUCCESS)
               return UNABLE_TO_OPEN_PRIMARY_INPUT;

            status = zvopen(unit, "OPEN_ACT", "", "OP","READ", NULL);
            if (status == FILE_IS_ALREADY_OPEN) { /* Already open, just copy */
               status = v2_check_primary_input(unit);
               if (status == SUCCESS)
                  return SUCCESS;	/* don't close if we didn't open! */
            }
            if (status == BAD_INPUT_LABEL)
               return status;
            if (status != SUCCESS)
               return UNABLE_TO_OPEN_PRIMARY_INPUT;

            status = zvclose(unit, NULL);
            if (status != SUCCESS)
               return UNABLE_TO_OPEN_PRIMARY_INPUT;
            return SUCCESS;
         }
         return BAD_INPUT_LABEL;	/* no file for that instance */
      }

      else {			/* Use given unit number */

         status = zvopen(primary_unit_requested, "OPEN_ACT", "", "OP","READ",
								NULL);
         if (status == FILE_IS_ALREADY_OPEN) { /* Already open, just copy */
            status = v2_check_primary_input(primary_unit_requested);
            if (status == SUCCESS)
               return SUCCESS;		/* don't close if we didn't open! */
         }
         if (status == BAD_INPUT_LABEL)
            return status;
         if (status != SUCCESS)
            return UNABLE_TO_OPEN_PRIMARY_INPUT;

         status = zvclose(primary_unit_requested, NULL);
         if (status != SUCCESS)
               return UNABLE_TO_OPEN_PRIMARY_INPUT;
         return SUCCESS;
      }
   }
   return SUCCESS;
}

