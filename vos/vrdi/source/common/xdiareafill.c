/*	xdiareafill- description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdiareafill( parameters )
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

FUNCTION FTN_NAME(xdiareafill)( Unit, Imp, X, Y, Boundary, Fill )
INTEGER	Unit, Imp, X, Y;
BYTE Boundary, Fill;
{
   return ( zdiareafill( *Unit, *Imp, *X, *Y, *Boundary, *Fill ) );
}

FUNCTION zdiareafill( unit, imp, x, y, boundary, fill )
int		unit, imp, x, y;
unsigned char	boundary, fill;
{
   int status, i;
   unsigned char *fill_line;
   struct RUNS_STRUCT {
      int x;
      int xplus;
      int y;
      int length;
   };
   struct RUNS_STRUCT *runs, *Segments;
   int max_runs;

   xd_current_call = IAREAFILL;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
   }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
   }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
   }
   else if (!ZCHECK_IMP(imp)) {
      status = NO_SUCH_IMP;
   }
   else if (XD_Out_Codes(x, y, ZAW(imp)) != 0) {
      status = NOT_IN_ACCESS_WINDOW;
   }
   else {
      SET_FLAG(ZIMP_FLAG( imp ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      status = XD_Device_Interface( &unit, FILL_REGION, &imp, ZAW(imp), 
                                    &x, &y, &boundary, &fill );

      if (status == DEVICE_CANNOT_DO_IT) {
         max_runs = 4 * ZN_SAMPS;
         fill_line = (unsigned char *) malloc( ZN_SAMPS );
         Segments = (struct RUNS_STRUCT *) 
                    malloc( max_runs * (sizeof (struct RUNS_STRUCT )) );

         if ( (fill_line == 0) || (Segments == 0) ) {
            status = MEMORY_ERROR;
            }
         else {
            for ( i = 0; i < ZN_SAMPS; i++ ) {
               fill_line[i] = fill;
            }

         status = XD_Get_Segments( &unit, &imp, &x, &y, &boundary, Segments,
                                   &max_runs );

         if ( status == SUCCESS ) {

            for ( runs = Segments; runs < (Segments + max_runs); runs++ ) {
               status = XD_Device_Interface( &unit, WRITE_LINE, imp, runs->x,
                           runs->y, runs->length, fill_line, ALL_BITS );
               if ( status != SUCCESS ) break;
               }
            }
         }
      }
   }
   if ( Segments != 0 ) free( Segments );
   if ( fill_line != 0 ) free( fill_line );

   xd_error_handler( &unit, status );

   return (status);
}
