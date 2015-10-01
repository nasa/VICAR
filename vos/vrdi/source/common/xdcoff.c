/*	xdcoff - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdcoff( parameters )
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

FUNCTION FTN_NAME(xdcoff)( Unit, Cursor )
INTEGER	Unit, Cursor;
   {
   return ( zdcoff( *Unit, *Cursor ) );
   }

FUNCTION zdcoff( unit, cursor )
int	unit, cursor;
   {
   int	status;

   xd_current_call = COFF;

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
      ZCURSOR_ACTIVE( cursor ) = FALSE;
      status = XD_Device_Interface( &unit, CURSOR_OFF, cursor );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
