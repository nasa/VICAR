/*	xdtrotate - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdtrotate( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include <math.h>

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdtrotate)( Angle )
REAL	Angle;
   {
   return ( zdtrotate( (double) *Angle ));
   }

FUNCTION zdtrotate( angle )
double	angle;
   {
   int		status;
   double	radian;

   xd_current_call = TROTATE;

   status = SUCCESS;
   if (!xd_initialized) {
      status = XD_Initialize();
      }

   if (status == SUCCESS) {
      radian = angle * PI / 180.0;
      FONT_SINE   = sin( radian );
      FONT_COSINE = cos( radian );
      }

   xd_error_handler( &NoUnit, status );
   return (status);
   }
