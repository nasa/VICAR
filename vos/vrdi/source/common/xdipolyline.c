/*	xdipolyline - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdipolyline( parameters )
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

FUNCTION FTN_NAME(xdipolyline)( Unit, Imp, Value, NPts, XCoord, YCoord )
INTEGER	Unit, Imp, NPts, XCoord, YCoord;
BYTE	Value;
   {
   return ( zdipolyline( *Unit, *Imp, *Value, *NPts, XCoord, YCoord ));
   }

FUNCTION zdipolyline( unit, imp, value, npts, xcoord, ycoord )
int		unit, imp, npts;
int		*xcoord, *ycoord;
unsigned char	value;
   {
   int	status;

   xd_current_call = IPOLYLINE;

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
   else if (npts < 2) {
      status = BAD_COORDINATE_COUNT;
      }
   else {
      SET_FLAG(ZIMP_FLAG( imp ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      status = XD_Polyline( &unit, imp, npts, xcoord, ycoord,
                            &value, ALL_BITS );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
