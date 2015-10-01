#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "errdefs.h"
#include "ftnbridge.h"

/* Return the current tape position (file number).  The unit must be	*/
/* open.  Returns -1 if unit is not a tape.				*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int FTN_NAME2(xvfilpos, XVFILPOS) (int *unit)	/* in: unit number to check */
{
   return zvfilpos(*unit);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvfilpos(int unit)			/* in: unit number to check */
{

#if RTL_USE_TAPE
   struct bufstate *bufstate;
#endif

   current_call = VFILPOS;

#if RTL_USE_TAPE

   if (v2_valid_unit(unit) != SUCCESS) {
      v2_error_handler(unit, NO_SUCH_UNIT);
      return 0;
   }

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN)) {
      v2_error_handler(unit, FILE_NOT_OPEN);
      return 0;
   }

   if (!(CURRENT_I_VALUE(FLAGS) & UNIT_IS_TAPE))
      return -1;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   if (bufstate->devstate.device_type != DEV_TAPE)
      return -1;

   return i_file[bufstate->devstate.dev.tape.tindex];

#else

   return -1;

#endif

}
