/*	xdcraw2imp - description
 *
 *	Purpose:     This routine translates a point from raw (screen) coordi-
 *                   nates to image plane coordinates for a given plane.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdcraw2imp( Unit, Imp, Xraw, Yraw, Ximp, Yimp )
 *
 *	Parameter List:
 *              Unit:  Unit number
 *              Imp:   Image plane number
 *              Xraw:  Raw (screen) x-coordinate (input)
 *              Yraw:  Raw (screen) y-coordinate (input)
 *              Ximp:  Image plane x-coordinate (output)
 *              Yimp:  Image plane y-coordinate (output)
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdroutines.h"

FTN_NAME(xdcraw2imp)( Unit, Imp, Xraw, Yraw, Ximp, Yimp )
INTEGER Unit, Imp, Xraw, Yraw, Ximp, Yimp;
{
   return ( zdcraw2imp (*Unit, *Imp, *Xraw, *Yraw, Ximp, Yimp ) );
}

zdcraw2imp(unit, imp, xraw, yraw, ximp, yimp)
int unit, imp, xraw, yraw;
int *ximp, *yimp;
{
   int zoom;			/* HW zoom factor			*/
   int status;

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
   else {
      zoom = ZZOOM( imp );	/* Compensate for hardware zoom		*/
      if (zoom == 0)
         zoom = 1;
   
      if (zoom > 0) {
         *ximp = (xraw-1) / zoom + ZDW_LEFT( imp );
         *yimp = (yraw-1) / zoom + ZDW_TOP( imp );
         }
      else {
         *ximp = (xraw-1) * (-zoom) + ZDW_LEFT( imp );
         *yimp = (yraw-1) * (-zoom) + ZDW_TOP( imp );
         }

      if (*ximp > ZN_SAMPS) *ximp -= ZN_SAMPS;	/* Compensate for image	*/
      if (*yimp > ZN_LINES) *yimp -= ZN_LINES;	/* planes that wrap	*/
      status = SUCCESS;
      }

   xd_error_handler( &unit, status );
   return (status);
}
