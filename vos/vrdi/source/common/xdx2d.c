/*	xdx2d - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdx2d( parameters )
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

FUNCTION FTN_NAME(xdx2d)( Unit, Device, XValue, YValue, Proximity, Pen )
INTEGER	Unit, Device;
REAL	XValue, YValue;
LOGICAL	Proximity, Pen;
   {
   return ( zdx2d( *Unit, *Device, XValue, YValue, Proximity, Pen ) );
   }

FUNCTION zdx2d( unit, device, xvalue, yvalue, proximity, pen )
int	unit, device;
float	*xvalue, *yvalue;
int	*proximity, *pen;
   {
   int	status;
   int	X, Y;

   xd_current_call = X2D;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZCHECK_IO_DEVICE( device )) {
      status = NO_SUCH_INPUT_DEVICE;
      }
   else if ((device == 1) && (ZIO_DEV_1_TYPE != DEVICE_2D)) {
      status = INVALID_DEVICE_TYPE;
      }
   else if ((device == 2) && (ZIO_DEV_2_TYPE != DEVICE_2D)) {
      status = INVALID_DEVICE_TYPE;
      }
   else {
      status = XD_Device_Interface( &unit, READ_2D, device, xvalue, yvalue,
                                    proximity, pen );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
