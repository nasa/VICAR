/*	xdtcolor - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdtcolor( parameters )
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

FUNCTION FTN_NAME(xdtcolor)( Color, Precision )
BYTE	Color;
INTEGER	Precision;
   {
   return ( zdtcolor( *Color, *Precision ));
   }

FUNCTION zdtcolor( color, precision )
unsigned char	color;
int		precision;
   {
   int		status;

   xd_current_call = TCOLOR;

   status = SUCCESS;
   if (!xd_initialized) {
      status = XD_Initialize();
      }

   if (status == SUCCESS) {
      if ((precision < 0) || (precision > 1)) {
         status = INVALID_TEXT_PREC;
         }
      else {
         if (precision == 0) {
            FONT_PRECISION = 1;
            }
         else {
            FONT_PRECISION = precision;
            }

         FONT_COLOR = color;
         status = SUCCESS;
         }
      }

   xd_error_handler( &NoUnit, status );
   return (status);
   }
