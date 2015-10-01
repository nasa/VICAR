/*	xdtsize - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdtsize( parameters )
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

FUNCTION FTN_NAME(xdtsize)( Height, Scale )
INTEGER	Height;
REAL	Scale;
   {
   return ( zdtsize( *Height, (double) *Scale ));
   }

FUNCTION zdtsize( height, scale )
int	height;
double	scale;
   {
   int	status;

   xd_current_call = TSIZE;

   status = SUCCESS;
   if (!xd_initialized) {
      status = XD_Initialize();
      }

   if (status == SUCCESS) {
      if (height <= 0) {
         status = INVALID_FONT_HEIGHT;
         }
      else if (scale <= 0.0) {
         status = INVALID_FONT_SCALE;
         }
      else {
         FONT_HEIGHT = height;
         FONT_SCALE  = scale;
         }
      }

   xd_error_handler( &NoUnit, status );
   return (status);
   }
