/*	xdclocation - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdclocation( parameters )
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

FUNCTION FTN_NAME(xdclocation)( Unit, Cursor, XPos, YPos )
INTEGER	Unit, Cursor, XPos, YPos;
   {
   return ( zdclocation( *Unit, *Cursor, XPos, YPos ) );
   }

FUNCTION zdclocation( unit, cursor, xpos, ypos )
int     unit, cursor;
int	*xpos, *ypos;
   {
   int  status;

   xd_current_call = CLOCATION;

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
      if ((ZAUTO_TRACK_CURSOR == 0) && (ZAUTO_TRACK_DEVICE == 0)) {
         *xpos = ZCURSOR_X(cursor);
         *ypos = ZCURSOR_Y(cursor);
         status = SUCCESS;
         }
      else {
         status = XD_Device_Interface( &unit, READ_CURSOR, cursor, xpos, ypos );
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
