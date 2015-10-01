/*	xdsgsection - description
 *
 *      Purpose:     Returns the section number of the graphics LUT currently 
 *                   in use.  Returns 1 on error.  (Duplicates xddinfo(35))
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		sect = xdsgsection( Unit )
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

FUNCTION FTN_NAME(xdsgsection)( Unit )
INTEGER Unit;
   {
   return ( zdsgsection( *Unit ) );
   }

FUNCTION zdsgsection( unit )
int     unit;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = 1;
      }
   else {
      status = ZOVERLAY_LUT_SECTION;
      }
   return (status);
   }
