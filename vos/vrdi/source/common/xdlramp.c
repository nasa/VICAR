/*	xdlramp - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdlramp( parameters )
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

FUNCTION FTN_NAME(xdlramp)( Unit, Lut, Section )
INTEGER	Unit, Lut, Section;
   {
   return ( zdlramp( *Unit, *Lut, *Section ));
   }

FUNCTION zdlramp( unit, lut, section )
int	unit, lut, section;
   {
   int	status, i, LookUpTable[256];

   xd_current_call = LRAMP;

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
   else if (!ZCHECK_LUT_SECTION( section )) {
      status = NO_SUCH_LUT_SECTION;
      }
   else {
      /*  Since we are modifying a look-up table, we set the flags to  */
      /*  indicate that the table has changed.  Also, since changing   */
      /*  the table may also change the mode, we set the mode flag,    */
      /*  set by xddconfigure(), to false.                             */

      ZVALID_MODE = FALSE;
      SET_FLAG(ZLUT_FLAG( lut ));
      SET_FLAG(ZLUT_FLAG( 0 ));

      for ( i = 0; i < 256; i++ ) {
         LookUpTable[i] = i * ZMAX_LUT_VALUE/255;
         }

      status = XD_Device_Interface(&unit, WRITE_LUT, lut, section, LookUpTable);
      }

   xd_error_handler( &unit, status );
   return (status);
   }
