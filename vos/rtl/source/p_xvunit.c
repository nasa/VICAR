#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xvunit(int *unit, char *name, int instance)
{
   int status;

   if (first_call)
      v2_general_initialize();		/* one-time setup */

   current_access = U;			/* Set up the current access */
   current_call = VUNI;

   status = v2_activate_a_unit(unit,instance,name);	/* Get a unit number */
   if (status != SUCCESS)
      return NO_FREE_UNITS;

   return SUCCESS;

}
