/*	xdgrgb - description
 *
 *	Purpose:
 *		Returns the DN value to use on the graphics plane to get
 *		as close as possible to the given RGB triplet color.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        July 26, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdgrgb( Unit, Red, Green, Blue )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *              Red:    Value for red (0-255)
 *              Green:  Value for green (0-255)
 *              Blue:   Value for blue (0-255)
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdgrgb)( Unit, Red, Green, Blue )
INTEGER	Unit, Red, Green, Blue;
   {
   return( zdgrgb( *Unit, *Red, *Green, *Blue ) );
   }

FUNCTION zdgrgb( unit, red, green, blue )
int	unit, red, green, blue;
   {
   int		dn, i, status;
   int		R[256], G[256], B[256];
   float	min, dist;
   
   if (!ZCHECK_UNIT_NUMBER) {
      dn = 0;
      }
   else if (!ZOVERLAY_AVAILABLE) {
      dn = 0;
      }
   else if ((red < 0) || (red > 255)) {
      dn = 0;
      }
   else if ((green < 0) || (green > 255)) {
      dn = 0;
      }
   else if ((blue < 0) || (blue > 255)) {
      dn = 0;
      }
   else {
      status = XD_Device_Interface( &unit, READ_OVERLAY_LUT, R, G, B,
                                    &ZOVERLAY_LUT_SECTION );

      if (status == SUCCESS) {
         for (i=0, min=3*SQUARED(256)+1; i < (1<<ZBITS_IN_OVERLAY); i++) {
            dist = SQUARED(abs(red-R[i])) + SQUARED(abs(green-G[i])) +
                                            SQUARED(abs(blue-B[i]));
            if (dist < min) {
               min = dist;
               dn = i;
               }
            }
         }
      else {
         dn = 0;
         }
      }
   return (dn);
   }

