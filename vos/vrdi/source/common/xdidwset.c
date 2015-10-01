/*	xdidwset - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdidwset( parameters )
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

FUNCTION FTN_NAME(xdidwset)( Unit, Imp, Left, Top )
INTEGER	Unit, Imp, Left, Top;
   {
   return( zdidwset( *Unit, *Imp, *Left, *Top ) );
   }

FUNCTION zdidwset( unit, imp, left, top )
int	unit, imp, left, top;
   {
   int	status, i;

   xd_current_call = IDWSET;

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
   else if (((left<1) || (left>ZN_SAMPS)) || ((top<1) || (top>ZN_LINES))) {
      status = BAD_DISPLAY_WINDOW;
      }
   else {
      ZDW_LEFT( imp ) = left;
      ZDW_TOP( imp ) = top;
      status = XD_Device_Interface( &unit, SET_DW, imp, left, top );
      if (status == MUST_SET_ALL_DWS) {
         for ( i = 1; i <= ZN_IMPS; i++ ) {
            ZDW_LEFT( i ) = left;
            ZDW_TOP( i ) = top;
            }
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
