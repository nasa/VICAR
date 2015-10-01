/*	xdsbypass - description
 *
 *      Purpose:     Given a lookup table number, returns TRUE (1) if bypass
 *                   is on (i.e., LUT not in use), and FALSE (0) if bypass is
 *                   off (LUT in use).  Returns FALSE (0) on error.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		bypass = xdsbypass( Unit, Lut )
 *
 *	Parameter List:
 *
 *		Unit:    Display device unit number
 *              Lut:     Lookup table number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdsbypass)( Unit, Lut )
INTEGER Unit, Lut;
   {
   return ( zdsbypass( *Unit, *Lut ) );
   }

FUNCTION zdsbypass( unit, lut )
int     unit, lut;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = 0;
      }
   else if (!ZCHECK_LUT( lut )) {
      status = 0;
      }
   else {
      status = ZBYPASS( lut );
      }
   return (status);
   }
