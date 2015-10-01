/*	xdglread - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdglread( parameters )
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

FUNCTION FTN_NAME(xdglread)( Unit, Section, Red, Green, Blue )
INTEGER	Unit, Section, Red, Green, Blue;
   {
   return( zdglread( *Unit, *Section, Red, Green, Blue ) );
   }

FUNCTION zdglread( unit, section, red, green, blue )
int	unit, section, *red, *green, *blue;
   {
   int	status;

   xd_current_call = GLREAD;

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
   else if ((section < 0) || (section > ZN_OVERLAY_SECTIONS)) {
      status = NO_SUCH_LUT_SECTION;
      }
   else {
      if (section == 0) {
         section = ZOVERLAY_LUT_SECTION;
         }
      status = XD_Device_Interface( &unit, READ_OVERLAY_LUT, red, green, blue,
                                    &section );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
