/*	xdsgbypass - description
 *
 *      Purpose:     Returns TRUE (1) if the graphics LUT is bypassed (i.e.,
 *                   LUT not in use), and FALSE (0) if bypass is off (LUT in
 *                   use).  Returns FALSE (0) on error.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		bypass = xdsgbypass( Unit )
 *
 *	Parameter List:
 *
 *		Unit:    Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdsgbypass)( Unit )
INTEGER Unit;
   {
   return ( zdsgbypass( *Unit ) );
   }

FUNCTION zdsgbypass( unit )
int     unit;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = 0;
      }
   else {
      status = ZOVERLAY_BYPASS;
      }
   return (status);
   }
