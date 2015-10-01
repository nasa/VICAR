/*	xdirotate - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdirotate( parameters )
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

#define buff(bline,bsample) buf[bline*length+bsample]

FUNCTION FTN_NAME(xdirotate)( Unit, Imp, Angle )
INTEGER	Unit, Imp, Angle;
{
   return( zdirotate( *Unit, *Imp, *Angle ) );
}

FUNCTION zdirotate( unit, imp, angle )
int	unit, imp, angle;
{
   int status;
   int windows_not_square,sample,row,column;
   int i,line,idx;
   int length;
   unsigned char *pix;
   unsigned char *buf;

   xd_current_call = IROTATE;

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
   else if ((angle < 1) || (angle > 4)) {
      status = INVALID_AW_ROTATION;
   }
   else {
      SET_FLAG(ZIMP_FLAG( imp ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      status = XD_Device_Interface( &unit, ROTATE_IMP, imp, ZAW(imp), angle );

      if (status == DEVICE_CANNOT_DO_IT) {
         windows_not_square = FALSE;
         status = SUCCESS;

         column = ZAW_RIGHT(imp)  - ZAW_LEFT(imp);
         row = ZAW_BOTTOM(imp) - ZAW_TOP(imp);
         if ( row != column ) {
            windows_not_square = TRUE;
            row = column = MIN( row, column );
            }
         length = row + 1;
         pix = (unsigned char *) malloc(length);
         buf = (unsigned char *) malloc(length*length);

         if ( (pix == 0) || (buf == 0) ) {
            status = MEMORY_ERROR;
            }
         if ( angle == 2 ) column = 0;
         for ( line = ZAW_TOP(imp); (line < (ZAW_TOP(imp)+length)) && 
                                    (status == SUCCESS); line++ ) {
            status = XD_Device_Interface( &unit, READ_LINE, imp, ZAW_LEFT(imp),
                                          line, length, pix );
            if (status != SUCCESS) break;

            switch (angle) {
               case 1: {   /* 180 */
                  for (sample=0, i=column; sample < length; sample++, i-- ) {
                     buff(row,i) = pix[sample];
                     }
                  row--;
                  break;
                  }
               case 2: {   /* -90  */
                  for (sample=0, i=row;	sample < length; sample++, i-- ) {
                     buff(i,column) = pix[sample];
                     }
                  column++;
                  break;
                  }
               case 3: {   /* 90   */
                  for ( sample = 0; sample < length; sample++ ) {
                     buff(sample,column) = pix[sample];
                     }
                  column--;
                  break;
                  }
               }
            }
         for ( line = ZAW_TOP(imp), idx = 0;
                (line < (ZAW_TOP(imp)+length)) && (status == SUCCESS);
                line++, idx+=length ) {
            status = XD_Device_Interface( &unit, WRITE_LINE, imp, ZAW_LEFT(imp),
                                          line, length, buf+idx, ALL_BITS );
            }
         if (pix != 0) free(pix);
         if (buf != 0) free(buf);

         if (windows_not_square && (status == SUCCESS)) {
            status = AW_NOT_SQUARE;
            }
      }
   }
   xd_error_handler( &unit, status );
   return (status);
}
