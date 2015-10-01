/*	xdihistogram - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdihistogram( parameters )
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

FUNCTION FTN_NAME(xdihistogram)( Unit, Imp, Mask, Histogram )
INTEGER	Unit, Imp, Mask, Histogram;
   {
   return( zdihistogram( *Unit, *Imp, *Mask, Histogram ) );
   }

FUNCTION zdihistogram( unit, imp, mask, histogram )
int	unit, imp, mask, *histogram;
   {
   int			status;
   unsigned char	*pixels, *m_pixels;
   int			line, m_line;
   int			lines, m_lines;
   int			length, m_length;
   int 			sample, dn, i;
   int 			windows_different;

   xd_current_call = IHISTOGRAM;

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
   else if ((mask != 0) && !ZCHECK_IMP( mask )) {
      status = NO_SUCH_IMP;
      }
   else {
      pixels = NULL;
      m_pixels = NULL;
      for (dn = 0; dn < 256; histogram[dn++] = 0);	/* Clear Histogram */

      status = XD_Device_Interface( &unit, COLLECT_HISTOGRAM, imp, mask,
                                    histogram, ZAW(imp), ZAW(mask) );

      if (status == DEVICE_CANNOT_DO_IT) {
         status = SUCCESS;
         length = ZAW_RIGHT(imp) - ZAW_LEFT(imp) + 1;
         lines  = ZAW_BOTTOM(imp) - ZAW_TOP(imp) + 1;

         if (mask != 0) {
            m_length = ZAW_RIGHT(mask) - ZAW_LEFT(mask) + 1;
            m_lines  = ZAW_BOTTOM(mask) - ZAW_TOP(mask) + 1;
            windows_different = (m_length != length) || (m_lines != lines);
            length = MIN(length,m_length);
            lines  = MIN(lines,m_lines);
            }

         pixels = (unsigned char *) malloc( length );
         if (pixels == NULL)
            status = MEMORY_ERROR;

         if (mask != 0) {
            m_pixels = (unsigned char *) malloc( length );
            if (m_pixels == NULL)
               status = MEMORY_ERROR;
            }

         for ( i = 0, line = ZAW_TOP(imp), m_line = ZAW_TOP(mask);
              (i < lines) && (status == SUCCESS); i++, line++, m_line++ ) {
            status = XD_Device_Interface( &unit, READ_LINE, imp, ZAW_LEFT(imp),
                                          line, length, pixels );
            if (status != SUCCESS) break;
            if (mask == 0) {
               for ( sample = 0; sample < length; sample++ ) {
                  histogram[pixels[sample]] += 1;
                  }
               }
            else {       /* mask != 0 */
               status = XD_Device_Interface( &unit, READ_LINE, mask, 
                                             ZAW_LEFT(mask), m_line, length,
                                             m_pixels );
               if (status != SUCCESS) break;
               for ( sample = 0; sample < length; sample++ ) {
                  if (m_pixels[sample] != 0) {
                     histogram[pixels[sample]] += 1;
                     }
                  }
               }
            }

         if ((mask != 0) && windows_different && (status == SUCCESS)) {
            status = AW_SIZES_DIFFERENT;
            }
         }
       
      if (pixels != NULL) free(pixels);
      if (m_pixels != NULL) free(m_pixels);
      }

   xd_error_handler( &unit, status );
   return (status);
   }
