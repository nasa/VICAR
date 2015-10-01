/*	xddinfo - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xddinfo( parameters )
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

FUNCTION FTN_NAME(xddinfo)( Unit, Start, Number, Array )
INTEGER	Unit, Start, Number, Array;
   {
   return ( zddinfo( *Unit, *Start, *Number, Array ));
   }

FUNCTION zddinfo( unit, start, number, array )
int	unit, start, number, *array;
   {
   int	status, i;

   xd_current_call = DINFO;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if ((start < 0) || ((start+number-1) > DCB_SIZE)) {
      status = INVALID_INFO_REQUEST;
      }
   else {
      status = SUCCESS;

      for ( i = 0; i < (number); i++ ) {
         array[i] = *(((int *)DCB[unit])+start+i);
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
