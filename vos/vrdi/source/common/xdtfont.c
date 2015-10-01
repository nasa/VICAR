/*	xdtfont - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdtfont( parameters )
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

FUNCTION FTN_NAME(xdtfont)( Font )
INTEGER	Font;
   {
   return ( zdtfont( *Font ));
   }

FUNCTION zdtfont( font )
int	font;
   {
   int		status;
 
   xd_current_call = TFONT;

   status = SUCCESS;
   if (!xd_initialized) {
      status = XD_Initialize();
      }

   if (status == SUCCESS) {
      if (font < 0) {
         FONT_NUMBER = 0;
         }
      else {
         FONT_NUMBER = font;
         }

      status = XD_Read_Font();
      }

   xd_error_handler( &NoUnit, status );
   return (status);
   }
