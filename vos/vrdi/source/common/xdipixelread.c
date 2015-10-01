/*	xdipixelread - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdipixelread( parameters )
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

FUNCTION FTN_NAME(xdipixelread)( Unit, Imp, XPos, YPos, Value )
INTEGER	Unit, Imp, XPos, YPos;
BYTE	Value;
   {
   return( zdipixelread( *Unit, *Imp, *XPos, *YPos, Value ) );
   }

FUNCTION zdipixelread( unit, imp, xpos, ypos, value )
int		unit, imp, xpos, ypos;
unsigned char	*value;
   {
   int	status;

   xd_current_call = IPIXELREAD;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZCHECK_IMP( imp )) {
      status = NO_SUCH_IMP;
      }
   else if (XD_Out_Codes(xpos, ypos, ZAW(imp)) != 0) {
      status = NOT_IN_ACCESS_WINDOW;
      }
   else {
      status = XD_Device_Interface( &unit, READ_PIXEL, imp, xpos, ypos, value );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
