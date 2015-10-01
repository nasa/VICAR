/*	xdiawset - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdiawset( parameters )
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

FUNCTION FTN_NAME(xdiawset)( Unit, Imp, Left, Top, Right, Bottom )
INTEGER	Unit, Imp, Left, Top, Right, Bottom;
   {
   return ( zdiawset( *Unit, *Imp, *Left, *Top, *Right, *Bottom ));
   }

FUNCTION zdiawset( unit, imp, left, top, right, bottom )
int	unit, imp, left, top, right, bottom;
   {
   int	status;

   xd_current_call = IAWSET;

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
   else if ((left > right) || (top > bottom)) {
      status = BAD_ACCESS_WINDOW;
      }
   else if ((left < 1) || (right > ZN_SAMPS)) {
      status = AW_NOT_ON_IMP;
      }
   else if ((top < 1) || (bottom > ZN_LINES)) {
      status = AW_NOT_ON_IMP;
      }
   else {
      ZAW_LEFT( imp )   = left;
      ZAW_TOP( imp )    = top;
      ZAW_RIGHT( imp )  = right;
      ZAW_BOTTOM( imp ) = bottom;

      status = SUCCESS;
      }

   xd_error_handler( &unit, status );
   return (status);
   }
