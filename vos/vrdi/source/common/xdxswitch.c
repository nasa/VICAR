/*	xdxswitch - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdxswitch( parameters )
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

FUNCTION FTN_NAME(xdxswitch)( Unit, Device, Switch, Value )
INTEGER	Unit, Device, Switch;
   {
   return ( zdxswitch( *Unit, *Device, *Switch, Value ) );
   }

FUNCTION zdxswitch( unit, device, switchnum, value )
int	unit, device, switchnum, *value;
   {
   int	status;

   xd_current_call = XSWITCH;

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
   else if (switchnum < 0) {
      status = NO_SUCH_SWITCH;
      }
   else if (((device == 1) && (switchnum >ZIO_DEV_1_N_SWITCHES)) ||
            ((device == 2) && (switchnum >ZIO_DEV_2_N_SWITCHES))) {
      status = NO_SUCH_SWITCH;
      }
   else {
      status = XD_Device_Interface( &unit, READ_SWITCH, device, switchnum,
                                    value );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
