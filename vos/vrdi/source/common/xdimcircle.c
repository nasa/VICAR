/*	xdimcircle - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdimcircle( parameters )
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

FUNCTION FTN_NAME(xdimcircle)( Unit, Imp, XCenter, YCenter, Radius, Mask, Value)
INTEGER	Unit, Imp, XCenter, YCenter, Radius;
BYTE	Mask, Value;
   {
   return ( zdimcircle( *Unit, *Imp, *XCenter, *YCenter, *Radius,
                        *Mask, *Value ));
   }

FUNCTION zdimcircle( unit, imp, xcenter, ycenter, radius, mask, value )
int		unit, imp, xcenter, ycenter, radius;
unsigned char	mask, value;
   {
   int	status;

   xd_current_call = IMCIRCLE;

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
   else if (radius < 1) {
      status = INVALID_RADIUS;
      }
   else {
      SET_FLAG(ZIMP_FLAG( imp ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      status = XD_Device_Interface( &unit, DRAW_CIRCLE, imp, xcenter, ycenter,
                                    radius, value, mask, ZAW(imp) );

      if (status == DEVICE_CANNOT_DO_IT) {
         status = XD_Circle( &unit, imp, xcenter, ycenter, radius,
                             &value, mask );
         }
      }

   xd_error_handler( unit, status );
   return (status);
   }
