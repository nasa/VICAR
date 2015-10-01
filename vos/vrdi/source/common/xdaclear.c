/*	xdaclear - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdaclear( Unit, X, Y, NChars )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
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

FUNCTION FTN_NAME(xdaclear)( Unit, X, Y, NChars )
INTEGER	Unit, X, Y, NChars;
   {
   return ( zdaclear( *Unit, *X, *Y, *NChars ) );
   }

FUNCTION zdaclear( unit, x, y, nchars )
int	unit, x, y, nchars;
   {
   int	status;

   xd_current_call = ACLEAR;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZAFG_AVAILABLE) {
      status = AFG_NOT_AVAILABLE;
      }
   else if (!ZAFG_ACTIVE) {
      status = AFG_NOT_ACTIVE;
      }
   else {
      status = XD_Device_Interface( &unit, AFG_CLEAR, x, y, nchars );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
