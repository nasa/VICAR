/*	xdcautotrack - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdcautotrack( parameters )
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

#define	DEFAULT_DEVICE	1

FUNCTION FTN_NAME(xdcautotrack)( Unit, Cursor, Device, Flag )
INTEGER	Unit, Cursor, Device;
LOGICAL	Flag;
   {
   return ( zdcautotrack( *Unit, *Cursor, *Device, *Flag ) );
   }

FUNCTION zdcautotrack( unit, cursor, device, flag )
int	unit, cursor, device, flag;
   {
   int	status;

   status = SUCCESS;
   xd_current_call = CAUTOTRACK;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZAUTO_TRACK_AVAILABLE) {
      status = NO_AUTO_TRACK;
      }
   else if (!ZCHECK_CURSOR( cursor )) {
      status = NO_SUCH_CURSOR;
      }
   else if ((device < 0) || (device > ZN_IO_DEVICES)) {
      status = NO_SUCH_INPUT_DEVICE;
      }
   else {
      if (!MAKE_LOGICAL( flag )) {
         if ((ZAUTO_TRACK_DEVICE != 0) && (ZAUTO_TRACK_CURSOR != 0)) {
            status = XD_Device_Interface( &unit, READ_CURSOR, 
                                          ZAUTO_TRACK_CURSOR,
                                          &ZCURSOR_X(ZAUTO_TRACK_CURSOR),
                                          &ZCURSOR_Y(ZAUTO_TRACK_CURSOR) );
            status = XD_Device_Interface( &unit, AUTO_OFF, ZAUTO_TRACK_DEVICE,
                                          ZAUTO_TRACK_CURSOR );

            ZAUTO_TRACK_DEVICE = 0;
            ZAUTO_TRACK_CURSOR = 0;
            }
         }
      else {
         if (device == 0) {
            ZAUTO_TRACK_DEVICE = DEFAULT_DEVICE;
            }
         else {
            ZAUTO_TRACK_DEVICE = device;
            }
         ZAUTO_TRACK_CURSOR = cursor;

         status = XD_Device_Interface( &unit, AUTO_ON, ZAUTO_TRACK_DEVICE,
                                       ZAUTO_TRACK_CURSOR );

         if ( status != SUCCESS ) {
            ZAUTO_TRACK_DEVICE = 0;
            ZAUTO_TRACK_CURSOR = 0;
            }
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }

