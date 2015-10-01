/*	xdlread - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdlread( parameters )
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

FUNCTION FTN_NAME(xdlread)( Unit, Lut, Section, Array )
INTEGER	Unit, Lut, Section, Array;
   {
   return ( zdlread( *Unit, *Lut, *Section, Array ));
   }

FUNCTION zdlread( unit, lut, section, array )
int	unit, lut, section, *array;
   {
   int	i, status;

   xd_current_call = LREAD;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZCHECK_LUT( lut )) {
      status = NO_SUCH_LUT;
      }
   else if ((section < 0) || (section > ZN_LUT_SECTIONS)) {
      status = NO_SUCH_LUT_SECTION;
      }
   else {
      if (section == 0) {
         if (ZBYPASS( lut )) {          /*  LUT bypass = TRUE  */
            for (i=0; i<256; i++)
               array[i] = i;
            status = SUCCESS;
            }
         else {                         /*  LUT bypass = FALSE  */
                                        /*  Use current section */
            section = ZWHICH_SECTION( lut );
            status = XD_Device_Interface(&unit, READ_LUT, lut, section, array);
            }
         }
      else {                            /*  Section not equal zero  */
         status = XD_Device_Interface( &unit, READ_LUT, lut, section, array );
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
