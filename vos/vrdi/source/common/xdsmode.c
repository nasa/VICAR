/*	xdsmode - description
 *
 *	Purpose:
 *		Returns the mode (bw/color/pseudo) that the device is
 *		currently in.  Returns monochrome (3) on error.  The values
 *		are the same as for XDDINFO(10), or the first element in
 *		the XDDCONFIGURE() array:
 *		1 = full color
 *		2 = pseudo-color
 *		3 = monochrome
 *		This routine does not exactly duplicate XDDINFO(10), but
 *		rather reads the look-up tables and the connections directly
 *		to determine the mode.
 *		monochrome--All three LUTs are connected to the same plane
 *			and all are identical.
 *		pseudo-color--All three LUTs are connected to the same plane,
 *			but are not identical.
 *		full color--The LUTs are connected to different planes, in
 *			any combination.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        August 14, 1989
 *
 *	Calling Sequence:
 *
 *		mode = xdsmode( Unit )
 *
 *	Parameter List:
 *
 *		Unit:    Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdsmode)( Unit )
INTEGER Unit;
   {
   return ( zdsmode( *Unit ) );
   }

FUNCTION zdsmode( unit )
int     unit;
   {
   int	i, lut, dispmode, status, lut1[256], lut2[256], invalid_unit=FALSE;

   if (!ZCHECK_UNIT_NUMBER) {
      dispmode = BLACK_AND_WHITE;
      invalid_unit = TRUE;
      }
   else if (ZVALID_MODE) {
      dispmode = ZOUTPUT_MODE;
      }
   else if (ZN_LUTS == 1) {
      dispmode = BLACK_AND_WHITE;
      }
   else {

      /*  First determine if all of the LUTs are connected to the same  */
      /*  image plane.  If not, then display mode is full color.        */

      for (lut=2, dispmode=BLACK_AND_WHITE; ((lut <= ZN_LUTS) &&
          (dispmode == BLACK_AND_WHITE)); lut++) {
         if (ZWHICH_IMP(1) != ZWHICH_IMP(lut))
            dispmode = FULL_COLOR;
         }

      if (dispmode != FULL_COLOR) {

         /*  The largest overhead in checking the mode is reading the    */
         /*  LUTs.  We read in the first LUT and compare it to the       */
         /*  other LUTs one at a time.  The display must be either black */
         /*  and white or pseudo-color.  If any of the array positions   */
         /*  differ, then the mode is pseudo-color--no need to check     */
         /*  further.                                                    */

         status = XD_Device_Interface(&unit,READ_LUT,1,ZWHICH_SECTION(1),lut1);

         for (lut=2, dispmode=BLACK_AND_WHITE; ((lut <= ZN_LUTS) &&
             (dispmode == BLACK_AND_WHITE) && (status == SUCCESS)); lut++) {

            status = XD_Device_Interface( &unit, READ_LUT, lut,
                                          ZWHICH_SECTION(lut), lut2 );
            if (status == SUCCESS)
               for (i=0; i < 256; i++)
                  if (lut1[i] != lut2[i]) {
                     dispmode = PSEUDO_COLOR;
                     break;
                     }
            }
         if (status != SUCCESS)    /*  Unable to read look-up table(s)  */
            dispmode = BLACK_AND_WHITE;
         }
      }

   if (!invalid_unit) {
      if ((!ZVALID_MODE) && (ZOUTPUT_MODE != dispmode))
         SET_FLAG( ZCONFIG_FLAG );
      ZOUTPUT_MODE = dispmode;
      ZVALID_MODE = TRUE;
      }
   return (dispmode);
   }
