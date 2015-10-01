/*	xdifill - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdifill( parameters )
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

FUNCTION FTN_NAME(xdifill)( Unit, Imp, Value )
INTEGER	Unit, Imp;
BYTE	Value;
   {
   return ( zdifill( *Unit, *Imp, *Value ));
   }

FUNCTION zdifill( unit, imp, value )
int		unit, imp;
unsigned char	value;
   {
   int		status, line, length, i;
   char		*pixels;

   xd_current_call = IFILL;

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
      SET_FLAG(ZIMP_FLAG( imp ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      status = XD_Device_Interface( &unit, FILL_AREA,
                                    imp, value, ALL_BITS, ZAW(imp) );
      if (status == DEVICE_CANNOT_DO_IT) {
         length = ZAW_RIGHT(imp) - ZAW_LEFT(imp) + 1;
         pixels = malloc( length );
         if (pixels == 0) {
            status = MEMORY_ERROR;
         }
         else {
            for ( i = 0; i < length; i++ ) {
               pixels[i] = value;
               }
            for ( line = ZAW_TOP(imp); line <= ZAW_BOTTOM(imp); line++ ) {
               status = XD_Device_Interface( &unit, WRITE_LINE,
                          imp, ZAW_LEFT(imp), line, length, pixels, ALL_BITS );
               if (status != SUCCESS) break;
               }
            free(pixels);
            }
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
