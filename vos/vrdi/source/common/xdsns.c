/*	xdsns - description
 *
 *      Purpose:     Returns the number of samples in the image planes.
 *                   Returns -1 on error.  (Duplicates xddinfo(6)).
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		ns = xdsns( Unit )
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

FUNCTION FTN_NAME(xdsns)( Unit )
INTEGER Unit;
   {
   return ( zdsns( *Unit ) );
   }

FUNCTION zdsns( unit )
int     unit;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = -1;
      }
   else {
      status = ZN_SAMPS;
      }
   return (status);
   }
