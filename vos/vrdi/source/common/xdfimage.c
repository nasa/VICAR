/*	xdfimage - description
 *
 *	Purpose:
 *
 *	Written by:  Paul Bartholomew
 *	Date:        July 11, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdfimage( Unit, Imp )
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

FUNCTION FTN_NAME(xdfimage)( Unit, Imp )
INTEGER Unit, Imp;
   {
   return ( zdfimage( *Unit, *Imp ) );
   }

FUNCTION zdfimage( unit, imp )
int     unit, imp;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = FALSE;
      }
   else if ((imp < 0) || (imp > ZN_IMPS)) {
      status = FALSE;
      }
   else {
      status = ZCHECK_FLAG( ZIMP_FLAG( imp ) );
      ZCLEAR_FLAG( ZIMP_FLAG( imp ) );
      }

   return (status != 0);
   }
