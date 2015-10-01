/*	xdizoom - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdizoom( parameters )
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

FUNCTION FTN_NAME(xdizoom)( Unit, Imp, Zoom )
INTEGER	Unit, Imp, Zoom;
   {
   return ( zdizoom( *Unit, *Imp, *Zoom ) );
   }

FUNCTION zdizoom( unit, imp, zoom )
int	unit, imp, zoom;
   {
   int	status, i;

   xd_current_call = IZOOM;

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
   else if (!ZMAY_ZOOM_IMPS) {
      status = CANNOT_ZOOM;
      }
   else if ((zoom > ZMAX_ZOOM_FACTOR) || (zoom < 1)) {
      status = CANNOT_ZOOM;
      }
   else {
      status = XD_Device_Interface( &unit, ZOOM_IMP, imp, zoom );

      if (status == MUST_ZOOM_ALL) {
         for ( i = 1; i <= ZN_IMPS; i++ ) {
            ZZOOM( i ) = zoom;
            }
         }

      if (status == SUCCESS) {
         ZZOOM( imp ) = zoom;
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
