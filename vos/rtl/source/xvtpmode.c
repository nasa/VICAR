#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "errdefs.h"
#include "ftnbridge.h"

/* Check to see if device is a tape (return 0 if not).			*/

/* Note: This used to return the density under VMS; however, nobody	*/
/* used the density and it is not easily available under Unix.  So, now	*/
/* it only returns 0/non-0.  Don't check for any specific non-0 number,	*/
/* as the return value may be changed to mean something in the future.	*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvtpmode, XVTPMODE) (int *unit, int *istape)
{
   *istape = zvtpmode(*unit);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvtpmode(int unit)
{

   current_call = VTPMODE;

#if RTL_USE_TAPE

   if (v2_valid_unit(unit) != SUCCESS) {
      v2_error_handler(unit, NO_SUCH_UNIT);
      return 0;
   }

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN)) {
      v2_error_handler(unit, FILE_NOT_OPEN);
      return 0;
   }

   if (CURRENT_I_VALUE(FLAGS) & UNIT_IS_TAPE)
      return 1;
   else
      return 0;

#else

   return 0;

#endif

}
