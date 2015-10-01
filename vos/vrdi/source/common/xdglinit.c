/*	xdglinit - description
 *
 *	Purpose:
 *		Initializes the graphics lookup table for the given section.
 *		The table is set up to allow the maximum number of colors
 *		that will easily fit on the display device.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        July 26, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdglinit( Unit, Section )
 *
 *	Parameter List:
 *
 *		Unit:     Display device unit number
 *		Section:  Section number of the graphics lookup table
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

FUNCTION FTN_NAME(xdglinit)( Unit, Section )
INTEGER	Unit, Section;
   {
   return( zdglinit( *Unit, *Section ) );
   }

FUNCTION zdglinit( unit, section )
int	unit, section;
   {
   int	i, status, R[256], G[256], B[256], maxval;

   xd_current_call = GLINIT;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZOVERLAY_AVAILABLE) {
      status = OVERLAY_NOT_AVAILABLE;
      }
   else if (!ZCHECK_OVERLAY_SECTION( section )) {
      status = NO_SUCH_LUT_SECTION;
      }
   else {
      SET_FLAG( ZGLUT_FLAG );
      ZDEFAULT_OVERLAY_LUT = TRUE;

      for (i = 0; i < 256; i++)
         R[i] = G[i] = B[i] = 0;

      /*  For an 8-bit graphics device, assign the bits in the order red,  */
      /*  green, blue, white, magenta, yellow, cyan, black and calculate   */
      /*  relative intensities where they overlap so we always get a unique*/
      /*  overlap color */

      if (ZBITS_IN_OVERLAY >= 8) {
         for (i = 0; i < 256; i++) {
            if (i & 0x01)   R[i] += 255;		/* red */
            if (i & 0x02)   G[i] += 255;		/* green */
            if (i & 0x04)   B[i] += 255;		/* blue */
            if (i & 0x08) { R[i] += 255;		/* white */
                            G[i] += 255;
                            B[i] += 255;  }
            if (i & 0x10) { R[i] += 255;		/* magenta */
                            B[i] += 255;  }
            if (i & 0x20) { R[i] += 255;		/* yellow */
                            G[i] += 255; }
            if (i & 0x40) { G[i] += 255;		/* cyan */
                            B[i] += 255;  }
            maxval = MAX(R[i], MAX(G[i], B[i]));

            if (maxval > 255) {			/* need to adjust range */
               R[i] *= (255.0 / (double)maxval);
               G[i] *= (255.0 / (double)maxval);
               B[i] *= (255.0 / (double)maxval);
               }
            if (i & 0x80) {			/* black */
               R[i] /= 2;			/* cut intensity in half */
               G[i] /= 2;
               B[i] /= 2;
               }
            } /* end for */
         } /* end if */

      else if (ZBITS_IN_OVERLAY >= 4) {
         for (i = 0; i < 16; i++) {
            if (i & 0x01)   R[i] += 255;	/* red */
            if (i & 0x02)   G[i] += 255;	/* green */
            if (i & 0x04)   B[i] += 255;	/* blue */
            if (i & 0x08) {			/* black */
               R[i] /= 2;	/* cut intensity in half */
               G[i] /= 2;
               B[i] /= 2;
               }
            } /* end for */
         } /* end if */

      status = XD_Device_Interface(&unit, WRITE_OVERLAY_LUT, R, G, B, &section);
      }

   xd_error_handler( &unit, status );
   return (status);
   }
