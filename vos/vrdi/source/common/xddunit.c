/*	xddunit - description
 *
 *	Purpose:
 *
 *	Written by:	S. Tews
 *	Date:		May 12, 1987
 *
 *	Calling Sequence:
 *
 *		STATUS = xddunit( Unit )
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
#include "xdalloc.h"

FUNCTION FTN_NAME(xddunit)( Unit )
INTEGER	Unit;
   {
   return ( zddunit( Unit ));
   }

FUNCTION zddunit( unit )
int	*unit;
   {
   int i, status; 
   int DevNumber;

   xd_current_call = DUNIT;

   status = SUCCESS;
   if (!xd_initialized) {
      status = XD_Initialize();
      }
   if (status == SUCCESS) {
      status = XD_Get_Device(&DevNumber, "");
      if (status == SUCCESS) {
         *unit = DevNumber;
         XD_Allocated[*unit] = TRUE;
         }
      }

   xd_error_handler ( &NoUnit, status );
   return ( status );
   }
