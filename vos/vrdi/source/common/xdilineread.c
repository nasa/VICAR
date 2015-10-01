/*	xdilineread - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdilineread( parameters )
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

FUNCTION FTN_NAME(xdilineread)( Unit, Imp, XPos, YPos, Size, Array )
INTEGER	Unit, Imp, XPos, YPos, Size;
BYTE	Array;
   {
   return ( zdilineread( *Unit, *Imp, *XPos, *YPos, *Size, Array ) );
   }

FUNCTION zdilineread( unit, imp, xpos, ypos, size, array )
int		unit, imp, xpos, ypos, size;
unsigned char	*array;
   {
   int	status, length, right, left;

   xd_current_call = ILINEREAD;

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
   else if ((ypos < ZAW_TOP(imp)) || (ypos > ZAW_BOTTOM(imp))) {
      status = SUCCESS;
      }
   else {
      left  = xpos;
      right = xpos + size - 1;
      left  = MAX( left,  ZAW_LEFT(imp) );
      right = MIN( right, ZAW_RIGHT(imp) );
      length = right - left + 1;

      if (length < 1) {
         status = SUCCESS;
         }
      else {
         status = XD_Device_Interface( &unit, READ_LINE, imp, left, ypos,
                                       length, array );
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
