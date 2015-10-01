/*	xdimlinewrite - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdimlinewrite( parameters )
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

FUNCTION FTN_NAME(xdimlinewrite)( Unit, Imp, XPos, YPos, Mask, Size, Array )
INTEGER	Unit, Imp, XPos, YPos, Size;
BYTE	Mask, Array;
   {
   return ( zdimlinewrite( *Unit, *Imp, *XPos, *YPos, *Mask, *Size, Array ) );
   }

FUNCTION zdimlinewrite( unit, imp, xpos, ypos, mask, size, array )
int		unit, imp, xpos, ypos, size;
unsigned char	mask, *array;
   {
   int	status, length, right, left;

   xd_current_call = IMLINEWRITE;

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
      SET_FLAG(ZIMP_FLAG( imp ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      left  = xpos;
      right = xpos + size - 1;
      left  = MAX( left,  ZAW_LEFT(imp) );
      right = MIN( right, ZAW_RIGHT(imp) );
      length = right - left + 1;

      if (length < 1) {
         status = SUCCESS;
         }
      else {
         status = XD_Device_Interface( &unit, WRITE_LINE, imp, left, ypos,
                                       length, array, mask );
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
