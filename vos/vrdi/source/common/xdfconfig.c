/*	xdfconfig - description
 *
 *	Purpose:
 *
 *	Written by:  Paul Bartholomew
 *	Date:        July 11, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdfconfig( Unit )
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
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdfconfig)( Unit )
INTEGER Unit;
   {
   return ( zdfconfig( *Unit ) );
   }

FUNCTION zdfconfig( unit )
int     unit;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = FALSE;
      }
   else {
      status = ZCHECK_FLAG( ZCONFIG_FLAG );
      ZCLEAR_FLAG( ZCONFIG_FLAG );
      }

   return (status != 0);
   }
