/*	xdlconnect - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdlconnect( parameters )
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

FUNCTION FTN_NAME(xdlconnect)( Unit, Imp, Lut, Section, Bypass )
INTEGER Unit, Imp, Lut, Section;
LOGICAL Bypass;
   {
   return ( zdlconnect( *Unit, *Imp, *Lut, *Section, *Bypass ));
   }

FUNCTION zdlconnect( unit, imp, lut, section, bypass )
int unit, imp, lut, section, bypass;
   {
   int	status, byp;

   xd_current_call = LCONNECT;

   byp = MAKE_LOGICAL(bypass);

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZCHECK_IMP( imp )) {
      status = NO_SUCH_IMP;
      }
   else if (!ZCHECK_LUT( lut )) {
      status = NO_SUCH_LUT;
      }
   else if (!ZCHECK_LUT_SECTION( section )) {
      status = NO_SUCH_LUT_SECTION;
      }
   else if (!ZMAY_CONNECT_IMP_LUT && (imp != ZWHICH_IMP(lut))) {
      status = CANNOT_CONNECT_LUT;
      }
   else if (ZOVERLAY_AVAILABLE &&
	    !ZMAY_CONNECT_IMP_OVERLAY && (imp == ZOVERLAY_IMP)) {
      status = CANNOT_CONNECT_LUT;
      }
   else {

      /*  Since we are changing a look-up table, the display mode set in  */
      /*  XDDCONFIGURE() is no longer valid.                              */

      ZVALID_MODE = FALSE;

      /*  If we change the section of the LUT or its bypass status, we    */
      /*  set the flag to indicate the LUT has changed.                   */

      if (ZWHICH_SECTION( lut ) != section) {
         ZSET_FLAG(ZLUT_FLAG( lut ));
         ZSET_FLAG(ZLUT_FLAG( 0 ));
         }

      /*  If the bypass flag is set, but we may not bypass the LUT, then  */
      /*  we do not set the flag because this is an invalid request--the  */
      /*  bypass status will not change.                                  */

      if ((ZBYPASS( lut ) != byp) && !(byp && !ZMAY_BYPASS_LUT)) {
         ZSET_FLAG(ZLUT_FLAG( lut ));
         ZSET_FLAG(ZLUT_FLAG( 0 ));
         }

      /*  If we connect the LUT to a different image plane, we set the    */
      /*  flag to indicate that the device configuration has changed.     */

      if (ZWHICH_IMP( lut ) != imp)
         ZSET_FLAG( ZCONFIG_FLAG );

      ZWHICH_IMP( lut ) = imp;
      ZBYPASS( lut ) = byp;

      if (byp) {
         ZWHICH_SECTION( lut ) = 0;
         }
      else {
         ZWHICH_SECTION( lut ) = section;
         }

      if (!ZMAY_CONNECT_IMP_LUT && (imp == ZWHICH_IMP(lut)))
         status = SUCCESS;		/* Can't connect, but already correct */
      else
         status = XD_Device_Interface( &unit, CONNECT_IMPS_LUTS, &imp, &lut, 
					&section, &byp);

      if ((status == SUCCESS) && byp && !ZMAY_BYPASS_LUT) {
         status = CANNOT_BYPASS_LUT;
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
