/*	xddactivate - Makes the specified unit active.
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xddactivate( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *		Flag:	Activate on/off flag
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xddactivate)( Unit, Flag )
INTEGER	Unit;
LOGICAL	Flag;
   {
   return ( zddactivate( *Unit, *Flag ) );
   }

FUNCTION zddactivate( unit, flag )
int	unit, flag;
   {
   int	status;

   xd_current_call = DACTIVATE;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else {
      ZDEV_ACTIVE = MAKE_LOGICAL( flag );
      status = SUCCESS;
      }

   xd_error_handler( &unit, status );
   return (status);
   }
