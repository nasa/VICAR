/*	xdiawlocation - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdiawlocation( parameters )
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

FUNCTION FTN_NAME(xdiawlocation)( Unit, Imp, Left, Top, Right, Bottom )
INTEGER	Unit, Imp, Left, Top, Right, Bottom;
   {
   return ( zdiawlocation( *Unit, *Imp, Left, Top, Right, Bottom ));
   }

FUNCTION zdiawlocation( unit, imp, left, top, right, bottom )
int	unit, imp, *left, *top, *right, *bottom;
   {
   int	status;

   xd_current_call = IAWLOCATION;

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
      *left   = ZAW_LEFT( imp );
      *top    = ZAW_TOP( imp );
      *right  = ZAW_RIGHT( imp );
      *bottom = ZAW_BOTTOM( imp );

      status = SUCCESS;
      }

   xd_error_handler( &unit, status );
   return (status);
   }
