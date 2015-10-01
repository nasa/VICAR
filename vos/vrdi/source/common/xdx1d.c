/*	xdx1d - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdx1d( parameters )
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

FUNCTION FTN_NAME(xdx1d)( Unit, Device, Knob, Value )
INTEGER	Unit, Device, Knob;
REAL	Value;
   {
   return ( zdx1d( *Unit, *Device, Knob, Value ) );
   }

FUNCTION zdx1d( unit, device, knob, value )
int	unit, device, *knob;
float	*value;
   {
   xd_current_call = X1D;

   return ( FUNC_NOT_IMPLEMENTED );
   }
