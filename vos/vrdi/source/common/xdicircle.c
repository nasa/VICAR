/*	xdicircle - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdicircle( parameters )
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

FUNCTION FTN_NAME(xdicircle)( Unit, Imp, XCenter, YCenter, Radius, Value )
INTEGER	Unit, Imp, XCenter, YCenter, Radius;
BYTE	Value;
   {
   return( zdicircle( *Unit, *Imp, *XCenter, *YCenter, *Radius, *Value ) );
   }

FUNCTION zdicircle( unit, imp, xcenter, ycenter, radius, value )
int		unit, imp, xcenter, ycenter, radius;
unsigned char	value;
   {
   int	status, batch_mode;

   xd_current_call = ICIRCLE;

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
      SET_FLAG( ZIMP_FLAG( imp ));
      SET_FLAG( ZIMP_FLAG( 0 ));
      status = XD_Device_Interface( &unit, DRAW_CIRCLE,
                                    imp, xcenter, ycenter, radius,
                                    value, ALL_BITS, ZAW(imp) );
      if (status == DEVICE_CANNOT_DO_IT) {
         batch_mode = ZBATCH_MODE;
         if (!batch_mode)
            zd_batch(unit, TRUE);
         status = XD_Circle( &unit, imp, xcenter, ycenter,
                             radius, &value, ALL_BITS );
         if (!batch_mode)
            zd_batch(unit, batch_mode);
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
