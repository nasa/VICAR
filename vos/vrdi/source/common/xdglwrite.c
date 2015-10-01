/*	xdglwrite - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdglwrite( parameters )
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

FUNCTION FTN_NAME(xdglwrite)( Unit, Section, Red, Green, Blue )
INTEGER	Unit, Section, Red, Green, Blue;
   {
   return ( zdglwrite( *Unit, *Section, Red, Green, Blue ) );
   }

FUNCTION zdglwrite( unit, section, red, green, blue )
int	unit, section, *red, *green, *blue;
   {
   int	status;

   xd_current_call = GLWRITE;
   
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
      ZDEFAULT_OVERLAY_LUT = FALSE;
      status = XD_Device_Interface( &unit, WRITE_OVERLAY_LUT, red, green, blue,
                                    &section );
      }
      
   xd_error_handler( &unit, status );
   return (status);
   }
