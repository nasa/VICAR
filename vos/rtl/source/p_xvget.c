#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xvget(int unit)
{

   if (first_call)
      v2_general_initialize();		/* one-time setup */

   current_access = G;			/* Set up the current access */
   current_call = VGET;

   if (v2_valid_unit(unit) != SUCCESS)
      return NO_SUCH_UNIT;

   return SUCCESS;

}
