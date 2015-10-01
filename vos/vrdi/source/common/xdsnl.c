/*	xdsnl - description
 *
 *	Purpose:     Returns the number of lines in the image plane.  Returns
 *                   -1 on error.  (Duplicates xddinfo(5)).
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		nl = xdsnl( Unit )
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

FUNCTION FTN_NAME(xdsnl)( Unit )
INTEGER Unit;
   {
   return ( zdsnl( *Unit ) );
   }

FUNCTION zdsnl( unit )
int     unit;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = -1;
      }
   else {
      status = ZN_LINES;
      }
    return (status);
   }
