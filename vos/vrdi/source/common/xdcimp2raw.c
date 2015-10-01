/*	xdcimp2raw - description
 *
 *	Purpose:     This routine translates a point from a given image plane 
 *                   coordinates to raw (screen) coordinates.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdcimp2raw( Unit, Imp, Ximp, Yimp, Xraw, Yraw)
 *
 *	Parameter List:
 *              Unit:  Unit number
 *              Imp:   Image plane number
 *              Ximp:  Image plane x-coordinate (input)
 *              Yimp:  Image plane y-coordinate (input)
 *              Xraw:  Raw (screen) x-coordinate (output)
 *              Yraw:  Raw (screen) y-coordinate (output)
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

FTN_NAME(xdcimp2raw)( Unit, Imp, Ximp, Yimp, Xraw, Yraw )
INTEGER Unit, Imp, Ximp, Yimp, Xraw, Yraw;
{
   return ( zdcimp2raw( *Unit, *Imp, *Ximp, *Yimp, Xraw, Yraw ) );
}

zdcimp2raw( unit, imp, ximp, yimp, xraw, yraw )
int  unit, imp, ximp, yimp;
int  *xraw, *yraw;
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
         *xraw = (ximp - ZDW_LEFT( imp )) * zoom + 1;
         *yraw = (yimp - ZDW_TOP( imp )) * zoom + 1;
         }
      else {
         *xraw = (ximp - ZDW_LEFT( imp )) / (-zoom) + 1;
         *yraw = (yimp - ZDW_TOP( imp )) / (-zoom) + 1;
         }

      while (*xraw <= 0) *xraw += ZN_SAMPS;	/* Compensate for image */
      while (*yraw <= 0) *yraw += ZN_LINES;	/* planes that wrap	*/
      while (*xraw > ZN_SAMPS) *xraw -= ZN_SAMPS;
      while (*yraw > ZN_LINES) *yraw -= ZN_LINES;
      status = SUCCESS;
      }

   xd_error_handler( &unit, status );
   return (status);
}
