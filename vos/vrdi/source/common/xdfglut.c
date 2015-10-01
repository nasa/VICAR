/*	xdfglut - description
 *
 *	Purpose:
 *
 *	Written by:  Paul Bartholomew
 *	Date:        July 11, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdfglut( Unit )
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

FUNCTION FTN_NAME(xdfglut)( Unit )
INTEGER	Unit;
   {
   return ( zdfglut( *Unit ) );
   }

FUNCTION zdfglut( unit )
int     unit;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = FALSE;
      }
   else {
      status = ZCHECK_FLAG( ZGLUT_FLAG );
      ZCLEAR_FLAG( ZGLUT_FLAG );
      }

   return (status != 0);
   }
