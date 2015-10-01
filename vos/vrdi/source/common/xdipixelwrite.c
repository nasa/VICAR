/*	xdipixelwrite - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdipixelwrite( parameters )
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

FUNCTION FTN_NAME(xdipixelwrite)( Unit, Imp, XPos, YPos, Value )
INTEGER	Unit, Imp, XPos, YPos;
BYTE	Value;
   {
   return ( zdipixelwrite( *Unit, *Imp, *XPos, *YPos, *Value ));
   }

FUNCTION zdipixelwrite( unit, imp, xpos, ypos, value )
int		unit, imp, xpos, ypos;
unsigned char	value;
   {
   int	status;

   xd_current_call = IPIXELWRITE;

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
   else if (XD_Out_Codes(xpos,ypos,ZAW(imp)) != 0) {
      status = NOT_IN_ACCESS_WINDOW;
      }
   else {
      SET_FLAG(ZIMP_FLAG( imp ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      status = XD_Device_Interface( &unit, WRITE_PIXEL, imp, xpos, ypos,
                                    &value, ALL_BITS );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
