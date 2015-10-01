/*	xdcsize - description
 *
 *	Purpose:  To change the size of the cursor.  If the current cursor
 *                form is not resizable, the cursor size global variables
 *		  will be updated, but the cursor will not be redrawn.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        November 17, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdcsize( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *              Cursor: Number of the cursor affected.  NOTE:  On some
 *			devices, two cursors may be needed to display a
 *			resizable cursor (i.e., one cursor in the upper
 *			left corner, the other in the lower right, to
 *			define a large box).  In this case, trying to use
 *			two resizable cursors will result in an error.
 *              XSize:  Horizontal size of the cursor
 *              YSize:  Vertical size of the cursor
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

FUNCTION FTN_NAME(xdcsize)( Unit, Cursor, XSize, YSize )
INTEGER	Unit, Cursor, XSize, YSize;
{
   return ( zdcsize( *Unit, *Cursor, *XSize, *YSize ) );
}

FUNCTION zdcsize( unit, cursor, xsize, ysize )
int	unit, cursor, xsize, ysize;
   {
   int	status, tmpForm;

   xd_current_call = CSIZE;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZMAY_RESIZE_CURSOR) {
      status = DEVICE_CANNOT_DO_IT;
      }
   else if (!ZCHECK_CURSOR( cursor )) {
      status = NO_SUCH_CURSOR;
      }
   else if (!ZCHECK_CURSOR_XSIZE( xsize )) {
      status = INVALID_CURSOR_SIZE;
      }
   else if (!ZCHECK_CURSOR_YSIZE( ysize )) {
      status = INVALID_CURSOR_SIZE;
      }
   else {
      ZCURSOR_XSIZE(cursor) = xsize;
      ZCURSOR_YSIZE(cursor) = ysize;
      if ((ZCURSOR_FORM(cursor) == -1) && (ZCURSOR_ACTIVE(cursor)))
         status = XD_Device_Interface( &unit, RESIZE_CURSOR, cursor, xsize,
                                       ysize);
      else
         status = SUCCESS;
      }

   xd_error_handler( &unit, status );
   return (status);
}
