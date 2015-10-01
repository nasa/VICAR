/*	xdglconstant - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdglconstant( parameters )
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

FUNCTION FTN_NAME(xdglconstant)( Unit, Section, Red, Green, Blue )
INTEGER	Unit, Section, Red, Green, Blue;
   {
   return( zdglconstant( *Unit, *Section, *Red, *Green, *Blue ) );
   }

FUNCTION zdglconstant( unit, section, red, green, blue )
int	unit, section, red, green, blue;
   {
   int	status, R[256], G[256], B[256], i;

   xd_current_call = GLCONSTANT;

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
      for ( i = 0; i < 256; i++ ) {
         R[i] = red;
         G[i] = green;
         B[i] = blue;
         }

      ZDEFAULT_OVERLAY_LUT = FALSE;
      SET_FLAG( ZGLUT_FLAG );
      status = XD_Device_Interface(&unit, WRITE_OVERLAY_LUT, R, G, B, &section);
      }

   xd_error_handler( &unit, status );
   return (status);
   }
