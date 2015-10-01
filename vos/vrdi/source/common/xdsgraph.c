/*	xdsgraph - description
 *
 *      Purpose:     Returns the image plane that is currently connected to the
 *                   graphics plane.  Returns 1 on error.  Duplicates 
 *                   xddinfo(34).
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		imp = xdsgraph( Unit )
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

FUNCTION FTN_NAME(xdsgraph)( Unit )
INTEGER Unit;
   {
   return ( zdsgraph( *Unit ) );
   }

FUNCTION zdsgraph( unit )
int     unit;
   {
   int	status;

    if (!ZCHECK_UNIT_NUMBER) {
      status = 1;
      }
   else {
      status = ZOVERLAY_IMP;
      }
    return (status);
   }
