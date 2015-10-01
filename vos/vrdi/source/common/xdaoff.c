/*	xdaoff - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdaoff( Unit )
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

FUNCTION FTN_NAME(xdaoff)( Unit )
INTEGER	Unit;
   {
   return ( zdaoff( *Unit ) );
   }

FUNCTION zdaoff( unit )
int	unit;
   {
   int	status;

   xd_current_call = AOFF;

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
   else {
      status = XD_Device_Interface( &unit, AFG_OFF );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
