/*	xdszoom - description
 *
 *      Purpose:     Returns the hardware zoom factor for the given image
 *                   plane.  Returns 1 on error.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		zoom = xdszoom( Unit, Imp )
 *
 *	Parameter List:
 *
 *		Unit:    Display device unit number
 *              Imp:     Image plane number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdszoom)( Unit, Imp )
INTEGER Unit, Imp;
   {
   return ( zdszoom( *Unit, *Imp ) );
   }

FUNCTION zdszoom( unit, imp )
int     unit,imp;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = 1;
      }
   else if (!ZCHECK_IMP( imp )) {
      status = 1;
      }
   else {
      status = ZZOOM( imp );
      }
   return (status);
   }
