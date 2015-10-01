/*	xdcshow - description
 *
 *	Purpose:
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdcshow( Unit, Cursor )
 *
 *	Parameter List:
 *
 *		Unit:    Display device unit number
 *              Cursor:  Cursor number
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

FUNCTION FTN_NAME(xdcshow)( Unit, Cursor )
INTEGER Unit, Cursor;
   {
   return ( zdcshow( *Unit, *Cursor ) );
   }

FUNCTION zdcshow( unit, cursor )
int     unit, cursor;
   {
   int	status;

   xd_current_call = CSHOW;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZCHECK_CURSOR( cursor )) {
      status = NO_SUCH_CURSOR;
      }
   else {
      ZCURSOR_ACTIVE(cursor) = TRUE;
      status = XD_Device_Interface( &unit, CURSOR_ON, cursor,
                                    ZCURSOR_FORM( cursor),
                                    ZCURSOR_BLINK( cursor) );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
