/*	xddutil - Opens/closes, moves and resizes devices 
 *
 *	Purpose: To make the operation of window systems automatic
 *
 *	Written by: Mark S. Mann
 *	Date:       October 18, 1988
 *
 *	     Calling Sequence:
 *		STATUS = xddopcls( Unit, open)
 *		STATUS = xddmove( Unit, xloc, yloc)
 *		STATUS = xddresize( Unit, width, height)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		open:	if TRUE open window, if FALSE close window
 *		xloc:	new x location
 *		yloc:	new y location
 *		width:	new width
 *		height:	new height
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xddopcls)( Unit, Open )
INTEGER	Unit, Open;
   {
   return( zddopcls( *Unit, *Open ));
   }

FUNCTION zddopcls( unit, open )
int	unit, open;
   {
   int status;

   xd_current_call = DOPCLS;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else {
      status = XD_Device_Interface( &unit, OPEN_CLOSE_WIN, open );
      }

   xd_error_handler( &unit, status );
   return (status);
   }

FUNCTION FTN_NAME(xddmove)( Unit, XLoc, YLoc )
INTEGER	Unit, XLoc, YLoc;
   {
   return( zddmove( *Unit, *XLoc, *YLoc ));
   }

FUNCTION zddmove( unit, xloc, yloc )
int	unit, xloc, yloc;
   {
   int status;

   xd_current_call = DMOVE;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else {
      status = XD_Device_Interface( &unit, MOVE_WIN, xloc, yloc );
      }

   xd_error_handler( &unit, status );
   return (status);
   }

FUNCTION FTN_NAME(xddresize)( Unit, Width, Height )
INTEGER	Unit, Width, Height;
   {
   return( zddresize( *Unit, *Width, *Height ));
   }

FUNCTION zddresize( unit, width, height )
int	unit, width, height;
   {
   int status;

   xd_current_call = DRESIZE;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else {
      status = XD_Device_Interface( &unit, RESIZE_WIN, width, height );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
