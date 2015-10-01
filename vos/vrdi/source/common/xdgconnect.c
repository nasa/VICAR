/*	xdgconnect - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdgconnect( parameters )
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

FUNCTION FTN_NAME(xdgconnect)( Unit, Imp, Section, Bypass )
INTEGER	Unit, Imp, Section;
LOGICAL	Bypass;
   {
   return ( zdgconnect( *Unit, *Imp, *Section, *Bypass ) );
   }

FUNCTION zdgconnect(unit, imp, section, bypass )
int unit, imp, section, bypass;
   {
   int	status, byp;

   xd_current_call = GCONNECT;

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
   else if (!ZCHECK_IMP( imp )) {
      status = NO_SUCH_IMP;
      }
   else if (!ZCHECK_OVERLAY_SECTION( section )) {
      status = NO_SUCH_LUT_SECTION;
      }
   else if (!ZMAY_CONNECT_IMP_OVERLAY && (imp != ZOVERLAY_IMP)) {
      status = CANNOT_CONNECT_LUT;
      }
   else {

      if (ZOVERLAY_LUT_SECTION != section)
         SET_FLAG(ZGLUT_FLAG);

      if (ZOVERLAY_IMP != imp)
         SET_FLAG(ZCONFIG_FLAG);

      byp = MAKE_LOGICAL(bypass);

      if ((ZOVERLAY_BYPASS != byp) && !(byp && !ZMAY_BYPASS_OVERLAY_LUT))
         SET_FLAG(ZGLUT_FLAG);

      ZOVERLAY_IMP = imp;
      ZOVERLAY_LUT_SECTION = section;
      ZOVERLAY_BYPASS = byp;

      status = XD_Device_Interface( &unit, CONNECT_OVERLAY, imp, section, byp );

      if ((status == SUCCESS) && (byp) && !ZMAY_BYPASS_OVERLAY_LUT) {
         status = CANNOT_BYPASS_LUT;
         }
      }

   xd_error_handler( &unit, status );
   return (status);
   }
