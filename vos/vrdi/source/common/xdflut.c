/*	xdflut - description
 *
 *	Purpose:
 *
 *	Written by:  Paul Bartholomew
 *	Date:        July 11, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdflut( Unit, Lut )
 *
 *	Parameter List:
 *
 *		Unit:    Display device unit number
 *              Lut:     Lookup Table Number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdflut)( Unit, Lut )
INTEGER	Unit, Lut;
   {
   return ( zdflut( *Unit, *Lut ) );
   }

FUNCTION zdflut( unit, lut )
int     unit, lut;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = FALSE;
      }
   else if ((lut < 0) || (lut > ZN_LUTS)) {
      status = FALSE;
      }
   else {
      status = ZCHECK_FLAG( ZLUT_FLAG( lut ) );
      ZCLEAR_FLAG( ZLUT_FLAG( lut ) );
      }

   return (status != 0);
   }
