/*	xdiilogical - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdiilogical( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include <stdio.h>
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdiilogical)( Unit, Operation, Imp1, Imp2, Imp3 )
INTEGER	Unit, Operation, Imp1, Imp2, Imp3;
   {
   return( zdiilogical( *Unit, *Operation, *Imp1, *Imp2, *Imp3 ) );
   }

FUNCTION zdiilogical( unit, operation, imp1, imp2, imp3 )
int	unit, operation, imp1, imp2, imp3;
   {
   int status;
   int windows_different,i,sample;
   int line1,line2,line3;
   int len1,len2,len3;
   int nl1,nl2,nl3;
   unsigned char *pix1,*pix2,*pix3;

   xd_current_call = IILOGICAL;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZCHECK_IMP(imp1) || !ZCHECK_IMP(imp2) || !ZCHECK_IMP(imp3)) {
      status = NO_SUCH_IMP;
      }
   else if ((operation < 0) || (operation > 5)) {
      status = UNDEFINED_OPERATION;
      }
   else {
      SET_FLAG(ZIMP_FLAG( imp3 ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      status = XD_Device_Interface( &unit, LOGICAL_OPERATION, operation,
                                    imp1, ZAW(imp1), imp2, ZAW(imp2),
                                    imp3, ZAW(imp3) );

      if (status == DEVICE_CANNOT_DO_IT) {
         windows_different = FALSE;
         status = SUCCESS;

         pix1 = NULL;
         pix2 = NULL;
         pix3 = NULL;

         len1 = ZAW_RIGHT(imp1) - ZAW_LEFT(imp1) + 1;
         len2 = ZAW_RIGHT(imp2) - ZAW_LEFT(imp2) + 1;
         len3 = ZAW_RIGHT(imp3) - ZAW_LEFT(imp3) + 1;
         if ((len1 != len2) || (len1 != len3) || (len2 != len3)) {
            windows_different = TRUE;
            len1 = MIN( len1, MIN(len2,len3) );
            }

         nl1 = ZAW_BOTTOM(imp1) - ZAW_TOP(imp1) + 1;
         nl2 = ZAW_BOTTOM(imp2) - ZAW_TOP(imp2) + 1;
         nl3 = ZAW_BOTTOM(imp3) - ZAW_TOP(imp3) + 1;
         if ((nl1 != nl2) || (nl1 != nl3) || (nl2 != nl3)) {
            windows_different = TRUE;
            nl1 = MIN( nl1, MIN(nl2,nl3) );
            }

         pix1 = (unsigned char *) malloc(len1);
         pix2 = (unsigned char *) malloc(len1);
         pix3 = (unsigned char *) malloc(len1);

         if ((pix1 == NULL) || (pix2 == NULL) || (pix3 == NULL)) {
            status = MEMORY_ERROR;
            }

         for ( i = 0, line1 = ZAW_TOP(imp1),
                      line2 = ZAW_TOP(imp2),
                      line3 = ZAW_TOP(imp3);
                      (i < nl1) && (status == SUCCESS);
                      i++, line1++, line2++, line3++ ) {
            status = XD_Device_Interface( &unit, READ_LINE, imp1,
                                          ZAW_LEFT(imp1), line1, len1, pix1 );
            if (status != SUCCESS) break;
            status = XD_Device_Interface( &unit, READ_LINE, imp2,
                                          ZAW_LEFT(imp2), line2, len1, pix2 );
            if (status != SUCCESS) break;
            for ( sample = 0; sample < len1; sample++ ) {
               switch (operation) {
                  case 0: {	/* AND */
                     pix3[sample] = pix2[sample] & pix1[sample];
                     break;
                     }
                  case 1: {	/* OR */
                     pix3[sample] = pix2[sample] | pix1[sample];
                     break;
                     }
                  case 2: {	/* XOR */
                     pix3[sample] = pix2[sample] ^ pix1[sample];
                     break;
                     }
                  case 3: {	/* NOT AND */
                     pix3[sample] = ~(pix2[sample] & pix1[sample]);
                     break;
                     }
                  case 4: {	/* NOT OR */
                     pix3[sample] = ~(pix2[sample] | pix1[sample]);
                     break;
                     }
                  case 5: {	/* NOT XOR */
                     pix3[sample] = ~(pix2[sample] ^ pix1[sample]);
                     break;
                     }
                  }
               }
            status = XD_Device_Interface( &unit, WRITE_LINE, imp3, ZAW_LEFT(imp3),
                                          line3, len1, pix3, ALL_BITS );
            if ( status != SUCCESS ) break;
            }

         if (pix1 != NULL) free(pix1);
         if (pix2 != NULL) free(pix2);
         if (pix3 != NULL) free(pix3);

         if (windows_different && (status == SUCCESS)) {
            status = AW_SIZES_DIFFERENT;
            }
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
