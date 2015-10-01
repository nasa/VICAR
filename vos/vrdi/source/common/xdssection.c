/*	xdssection - description
 *
 *      Purpose:     Given a lookup table number, returns the section number
 *                   of the LUT that is currently in use.  Returns 1 on error.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		sect = xdssection( Unit, Lut )
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

FUNCTION FTN_NAME(xdssection)( Unit, Lut )
INTEGER Unit, Lut;
   {
   return ( zdssection( *Unit, *Lut ) );
   }

FUNCTION zdssection( unit, lut )
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
      status = ZWHICH_SECTION( lut );
      }
    return (status);
   }
