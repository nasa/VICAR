/*	xdidwlocation - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdidwlocation( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
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

FUNCTION FTN_NAME(xdidwlocation)( Unit, Imp, Left, Top )
INTEGER	Unit, Imp, Left, Top;
   {
   return( zdidwlocation( *Unit, *Imp, Left, Top ) );
   }

FUNCTION zdidwlocation( unit, imp, left, top )
int	unit, imp, *left, *top;
   {
   int	status;

   xd_current_call = IDWLOCATION;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZCHECK_IMP( imp )) {
      status = NO_SUCH_IMP;
      }
   else {
      *left = ZDW_LEFT( imp );
      *top  = ZDW_TOP( imp );

      status = SUCCESS;
      }

   xd_error_handler( &unit, status );
   return (status);
   }
