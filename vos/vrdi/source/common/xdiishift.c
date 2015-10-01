/*	xdiishift - description
 *
 *	Purpose:
 *
 *	Written by:  
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdiishift( parameters )
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

FUNCTION FTN_NAME(xdiishift)( Unit, Shift, Imp1, Imp2, Wrap )
INTEGER	Unit, Shift, Imp1, Imp2;
LOGICAL	Wrap;
   {
   return( zdiishift( *Unit, *Shift, *Imp1, *Imp2, *Wrap ) );
   }

FUNCTION zdiishift( unit, shift, imp1, imp2, wrap )
int	unit, shift, imp1, imp2, wrap;
{
   int status;
   int windows_different,i,sample;
   int line1,line2;
   int len1,len2;
   int nl1,nl2;
   unsigned char *pix1,*pix2;

   static char SIGN_BIT = (char)0x80;
   static int Shift_Right = FALSE;
   int Opp_Shift, Num_Bits;
   unsigned char Mask;

   xd_current_call = IISHIFT;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
     status = DEVICE_NOT_OPEN;
     }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if ( !ZCHECK_IMP(imp1) || !ZCHECK_IMP(imp2) ) {
      status = NO_SUCH_IMP;
      }
   else {
      SET_FLAG(ZIMP_FLAG( imp2 ));
      SET_FLAG(ZIMP_FLAG( 0 ));
      status = XD_Device_Interface( &unit, SHIFT_IMP, shift, wrap,
                                    imp1, ZAW(imp1), imp2, ZAW(imp2) );

      if (status == DEVICE_CANNOT_DO_IT) {
         windows_different = FALSE;
         status = SUCCESS;

         pix1 = NULL;
         pix2 = NULL;

         len1 = ZAW_RIGHT(imp1) - ZAW_LEFT(imp1) + 1;
         len2 = ZAW_RIGHT(imp2) - ZAW_LEFT(imp2) + 1;
         if (len1 != len2) {
            windows_different = TRUE;
            len1 = MIN( len1, len2 );
            }

         nl1 = ZAW_BOTTOM(imp1) - ZAW_TOP(imp1) + 1;
         nl2 = ZAW_BOTTOM(imp2) - ZAW_TOP(imp2) + 1;
         if (nl1 != nl2) {
            windows_different = TRUE;
            nl1 = MIN( nl1, nl2 );
            }

         pix1 = (unsigned char *) malloc(len1);
         pix2 = (unsigned char *) malloc(len1);

         if ( (pix1 == NULL) || (pix2 == NULL) ) {
            status = MEMORY_ERROR;
            }
         Num_Bits = shift;
         if ( Num_Bits < 0 ) {
            Num_Bits = -Num_Bits;
            Shift_Right = TRUE;
            }
         Num_Bits = MIN( 8, Num_Bits);
         Opp_Shift = 8 - Num_Bits;
         Mask = SIGN_BIT >> (Num_Bits - 1);

         for ( i = 0, line1 = ZAW_TOP(imp1),
                      line2 = ZAW_TOP(imp2);
                      (i < nl1) && (status == SUCCESS);
                      i++, line1++, line2++ ) {
            status = XD_Device_Interface( &unit, READ_LINE, imp1,
                                          ZAW_LEFT(imp1), line1, len1, pix1 );
            if (status != SUCCESS) break;
            for ( sample = 0; sample < len1; sample++ ) {

               if ( Shift_Right ) {
                  if ( wrap ) {
                     pix2[sample] = (((Mask >> Opp_Shift) & pix1[sample]) 
                                     << Opp_Shift) | (pix1[sample] >> Num_Bits);
                     }
                  else pix2[sample] = pix1[sample] >> Num_Bits;
                  }
               else {
                  if ( wrap ) {
                     pix2[sample] = ((Mask & pix1[sample]) >> Opp_Shift) | 
                                     (pix1[sample] << Num_Bits);
                     }
                  else pix2[sample] = pix1[sample] << Num_Bits;
                  }
	       }
            status = XD_Device_Interface( &unit, WRITE_LINE, imp2,
                                          ZAW_LEFT(imp2), line2, len1, pix2,
                                          ALL_BITS );
            if ( status != SUCCESS ) break;
            }

         if (pix1 != NULL) free(pix1);
         if (pix2 != NULL) free(pix2);

         if (windows_different && (status == SUCCESS)) {
            status = AW_SIZES_DIFFERENT;
            }
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
