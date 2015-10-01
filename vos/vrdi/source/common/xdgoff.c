/*	xdgoff - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdgoff( parameters )
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

FUNCTION FTN_NAME(xdgoff)( Unit )
INTEGER	Unit;
   {
   return ( zdgoff( *Unit ) );
   }

FUNCTION zdgoff( unit )
int	unit;
   {
   int	status;

   xd_current_call = GOFF;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZOVERLAY_AVAILABLE) {
      status = OVERLAY_NOT_AVAILABLE;
      }
   else {
      ZOVERLAY_ON = FALSE;
      status = XD_Device_Interface( &unit, GRAPHICS_OFF );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
