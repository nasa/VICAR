#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xvread(int unit)
{

   if (first_call)
      v2_general_initialize();		/* one-time setup */

   current_access = R;			/* Set up the current access */
   current_call = VREA;

   if (v2_valid_unit(unit) != SUCCESS)
      return NO_SUCH_UNIT;

   v2_initialize_value_table(unit_table, N_UNIT_TABLE_ENTRIES,
			current_table[unit],
			default_table);

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN))
      return FILE_NOT_OPEN;

   if (!EQUAL(CURRENT_S_VALUE(OP),"READ")&&!EQUAL(CURRENT_S_VALUE(OP),"UPDATE"))
      return IMPROPER_OPERATION;

   return SUCCESS;

}
