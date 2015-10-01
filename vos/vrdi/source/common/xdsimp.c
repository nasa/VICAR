/*	xdsimp - description
 *
 *      Purpose:     Given a lookup table number, returns the image plane
 *                   that is currently connected to it.  Returns 1 on error.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		imp = xdsimp( Unit, Lut )
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

FUNCTION FTN_NAME(xdsimp)( Unit, Lut )
INTEGER Unit, Lut;
   {
   return ( zdsimp( *Unit, *Lut ) );
   }

FUNCTION zdsimp( unit, lut )
int     unit, lut;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = 1;
      }
   else if (!ZCHECK_LUT( lut )) {
      status = 1;
      }
   else {
      status = ZWHICH_IMP( lut );
      }
   return (status);
   }
