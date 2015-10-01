/*	xdccolor - description
 *
 *	Purpose:  To change the color of the cursor.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        December 7, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdccolor( Unit, Cursor, Red, Green, Blue )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *              Cursor: Number of the cursor affected.
 *		Red:	Red value of the color (0-255)
 *		Green:	Green value of the color (0-255)
 *		Blue:	Blue value of the color (0-255)
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

FUNCTION FTN_NAME(xdccolor)( Unit, Cursor, Red, Green, Blue )
INTEGER	Unit, Cursor, Red, Green, Blue;
{
   return ( zdccolor( *Unit, *Cursor, *Red, *Green, *Blue ) );
}

FUNCTION zdccolor( unit, cursor, red, green, blue )
int	unit, cursor, red, green, blue;
   {
   int	status, tmpForm;

   xd_current_call = CCOLOR;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZMAY_COLOR_CURSOR) {
      status = DEVICE_CANNOT_DO_IT;
      }
   else if (!ZCHECK_COLOR( red )) {
      status = INVALID_COLOR;
      }
   else if (!ZCHECK_COLOR( green )) {
      status = INVALID_COLOR;
      }
   else if (!ZCHECK_COLOR( blue )) {
      status = INVALID_COLOR;
      }
   else if (!ZCHECK_CURSOR( cursor )) {
      status = NO_SUCH_CURSOR;
      }
   else {
      ZCURSOR_RED(cursor) = red;
      ZCURSOR_GREEN(cursor) = green;
      ZCURSOR_BLUE(cursor) = blue;
      if (ZCURSOR_ACTIVE(cursor))
         status = XD_Device_Interface( &unit, COLOR_CURSOR, cursor, red,
                                       green, blue );
      else
         status = SUCCESS;
      }

   xd_error_handler( &unit, status );
   return (status);
}
