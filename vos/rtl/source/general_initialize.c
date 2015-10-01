#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Performs one-time initialization upon the first call to an xv routine */

void v2_general_initialize(void)
{
   int i;

   v2_get_def_filename();	/* Use process id chars to make a	*/
				/* unique default file name postfix.	*/

   for (i=0; i<N_ACTIVE_UNITS_ALLOWED; i++) { 
      active_units[i].unit = V2_INACTIVE;
      active_units[i].was_once_open = FALSE;
      current_table[i] = NULL;
      label_table[i] = NULL;
   }

   first_call = FALSE;
}

