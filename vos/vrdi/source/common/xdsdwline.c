/*	xdsdwline - description
 *
 *      Purpose:     Returns the line number of the upper left corner of the
 *                   display window (pan) for the given plane.  Returns 1 on
 *                   error.  Duplicates half of xdidwlocation().
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		pantop = xdsdwline( Unit, Imp )
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

FUNCTION FTN_NAME(xdsdwline)( Unit, Imp )
INTEGER Unit, Imp;
   {
   return ( zdsdwline( *Unit, *Imp ) );
   }

FUNCTION zdsdwline( unit, imp )
int     unit, imp;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = 1;
      }
   else if (!ZCHECK_IMP( imp )) {
      status = 1;
      }
   else {
      status = ZDW_TOP( imp );
      }
   return (status);
   }
