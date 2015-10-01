#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xlinfo(int unit)
{

   if (first_call)
      v2_general_initialize();		/* one-time setup */

   current_access = I;			/* Set up the current access */
   current_call = LINF;

   if (v2_valid_unit(unit) != SUCCESS)
      return NO_SUCH_UNIT;

   if (SEQ_DEVICE && (CURRENT_I_VALUE(FLAGS) & DATA_WRITTEN))
      return TOO_LATE;

/* The rest of the checks were moved to c_xlinfo so err_act would work */

   v2_initialize_value_table(label_options,N_LABEL_TABLE_ENTRIES,
			label_table[unit],
			label_default_table);
   v2_initialize_value_table(unit_table, N_UNIT_TABLE_ENTRIES,
			current_table[unit],
			default_table);

   return SUCCESS;

}
