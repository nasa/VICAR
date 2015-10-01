/*	xdx3d - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdx3d( parameters )
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

FUNCTION FTN_NAME(xdx3d)( Unit, Device, XValue, YValue, ZValue, Proximity, Pen )
INTEGER	Unit, Device;
REAL	XValue, YValue, ZValue;
LOGICAL	Proximity, Pen;
   {
   return( zdx3d( *Unit, *Device, XValue, YValue, ZValue, Proximity, Pen ) );
   }

FUNCTION zdx3d( unit, device, xvalue, yvalue, zvalue, proximity, pen )
int	unit, device;
float	*xvalue, *yvalue, *zvalue;
int	*proximity, *pen;
   {
   xd_current_call = X3D;

   return ( FUNC_NOT_IMPLEMENTED );
   }
