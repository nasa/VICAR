/*	xdcset - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdcset( parameters )
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

FUNCTION FTN_NAME(xdcset)( Unit, Cursor, XPos, YPos )
INTEGER	Unit, Cursor, XPos, YPos;
   {
   return ( zdcset( *Unit, *Cursor, *XPos, *YPos ) );
   }


FUNCTION zdcset( unit, cursor, xpos, ypos )
int     unit, cursor, xpos, ypos;
   {
   int	status;
    
   xd_current_call = CSET;

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
   else if ((xpos < 1) || (xpos > ZVIDEO_SAMPLES) ||
            (ypos < 1) || (ypos > ZVIDEO_LINES)) {
      status = INVALID_COORDINATES;
      }
   else {
      ZCURSOR_X(cursor) = xpos;
      ZCURSOR_Y(cursor) = ypos;
      status = XD_Device_Interface( &unit, WRITE_CURSOR, cursor, xpos, ypos );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
