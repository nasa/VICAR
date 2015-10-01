/*	IVAS_Vector.c - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Vector.c( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "ivasinc.h"

FUNCTION IVAS_Vector( Unit, imp, npts, x, y, color, mask )
int	*Unit, imp, npts, *x, *y, color, mask;
{
   extern double sqrt();
   struct {
     int XY_x;
     int XY_y;
   } XY_xy[1024];
   struct {
     int OFF_x;
     int OFF_y;
   } OFF_xy;

   float slope, intercept;
   int	i, ipt, sign, mpts, xdiff, ydiff;

   if (imp == OVERLAY_IMP) {
      IVASgphValue( color, color );
      IVASgphMask( mask );
      IVASgphMove( IVAS_X_GPH(x[0]), IVAS_Y_GPH(y[0]), GPHabs );
      for ( i = 1; i < npts; i++ ) {
         IVASgphLine( IVAS_X_GPH(x[i]), IVAS_Y_GPH(y[i]), GPHabs );
         }
   }
   else {
      OFF_xy.OFF_x = 0;
      OFF_xy.OFF_y = 0;
      for (i=1; i < npts; i++) {
         sign = 1;
         xdiff = x[i] - x[i-1];
         ydiff = y[i] - y[i-1];

         if ((xdiff == 0) && (ydiff == 0)) {		/*  Single point  */
            XY_xy[0].XY_x = x[i-1] - 1;
            XY_xy[0].XY_y = y[i-1] - 1;
            mpts = 1;
         }

         else if (xdiff == 0) {				/*  Vertical line  */
            if (ydiff < 0)
              sign = -1;
	    mpts = ABS(ydiff) + 1;
            for (ipt=0; ipt < mpts; ipt++) {
               XY_xy[ipt].XY_x = x[i-1] - 1;
               XY_xy[ipt].XY_y = (y[i-1] + (sign*ipt)) - 1;
            }
         }

         else if (ydiff == 0) {				/*  Horizontal line  */
            if (xdiff < 0)
              sign = -1;
            mpts = ABS(xdiff) + 1;
            for (ipt=0; ipt < mpts; ipt++) {
               XY_xy[ipt].XY_x = (x[i-1] + (sign*ipt)) - 1;
               XY_xy[ipt].XY_y = y[i-1] - 1;
            }
         }

         else {
            slope = (float)ydiff / (float)xdiff;
            intercept = (float)y[i] - ((float)x[i] * slope);

            XY_xy[0].XY_x = x[i-1] - 1;
            XY_xy[0].XY_y = y[i-1] - 1;

            if (ABS(xdiff) > ABS(ydiff)) {
               if (xdiff < 0)
                 sign = -1;
               mpts = ABS(xdiff) + 1;
               for (ipt=1; ipt < mpts; ipt++) {
                  XY_xy[ipt].XY_x = (x[i-1] + (sign*ipt)) - 1;
                  XY_xy[ipt].XY_y = (int)((slope*XY_xy[ipt].XY_x)+intercept);
               }
            }
            else {
               if (ydiff < 0)
                 sign = -1;
               mpts = ABS(ydiff) + 1;
               for (ipt=1; ipt < mpts; ipt++) {
                  XY_xy[ipt].XY_y = (y[i-1] + (sign*ipt)) - 1;
                  XY_xy[ipt].XY_x = (int)(((float)XY_xy[ipt].XY_y - 
                                           intercept)/slope);
               }
            }
         }
         IVASSendBuffer(&color,1,XY_xy,mpts,&OFF_xy,imp-1,mask);
      }
   }
   IVASflush();
   return (SUCCESS);
}

/*  This routine was lifted "as is" from the unixstubs.c program that is  */
/*  part of the IVAS system libraries.  We did not use the entire unix-   */
/*  stubs.c program because many of the routines are duplicated in the    */
/*  ivasstubs.c program, also part of the IVAS system libraries.  (It is  */
/*  called IVASmaWriteRandom() in the system library.)                    */

IVASSendBuffer(data, count, xy, num, xyOff, memSelect, planeMask)
int *data, count;
XYint *xy;
int num;
XYint *xyOff;
int memSelect, planeMask;
{
   InitBuffer (MaxParm, 12 + (count*4)/2 + (num*8)/2);
   PackHeader (0, 10, 5, 0);
   PackInt (count);
   PackInt (num);
   PackIntArray (data, count);
   PackXYint (xy, num);
   PackXYint (xyOff, 1);
   PackInt (memSelect);
   PackInt (planeMask);
   SendBuffer;
   return (SUCCESS);
}
