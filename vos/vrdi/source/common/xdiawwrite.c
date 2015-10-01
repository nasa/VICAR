/*	xdiawwrite - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdiawwrite( parameters )
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

FUNCTION FTN_NAME(xdiawwrite)( Unit, Imp, Size, Array )
INTEGER	Unit, Imp, Size;
BYTE	Array;
   {
   return ( zdiawwrite( *Unit, *Imp, *Size, Array ) );
   }

FUNCTION zdiawwrite( unit, imp, size, array )
int		unit, imp, size;
unsigned char	*array;
   {
   int		status, line, length;
   char		*pixelAddress, *endAddress;

   xd_current_call = IAWWRITE;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else  if (!ZCHECK_IMP( imp )) {
      status = NO_SUCH_IMP;
      }
   else if (size < 1) {
      status = BAD_PIXEL_COUNT;
      }
   else {
      SET_FLAG(ZIMP_FLAG( imp ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      status = XD_Device_Interface( &unit, WRITE_AREA, imp, size, ZAW(imp),
                                    array, ALL_BITS );

      if (status == DEVICE_CANNOT_DO_IT) {
         pixelAddress = (char *) array;
         endAddress = (char *) (array + size - 1);
         length = ZAW_RIGHT(imp) - ZAW_LEFT(imp) + 1;
         for ( line = ZAW_TOP(imp); line <= ZAW_BOTTOM(imp); line++ ) {
            status = XD_Device_Interface( &unit, WRITE_LINE,
                                          imp, ZAW_LEFT(imp), line,
                                          length, pixelAddress, ALL_BITS );
            if (status != SUCCESS) break;
            pixelAddress += length;
            if (pixelAddress >= endAddress) break;
            }
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
