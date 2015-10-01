/*	IVAS_Cursor - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Cursor( parameters )
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

FUNCTION IVAS_Cursor(Unit,function,cursor,form,rate,xsize,ysize,red,green,blue)
int	*Unit, function, cursor, form, rate, xsize, ysize, red, green, blue;
{
   struct {
      int IVAS_x;
      int IVAS_y;
   } IVAS_xy;

   int	IVAS_Rate_On, IVAS_Rate_Off, ix, iy, ix2, iy2;
   int  button, x, y, color=0;

   color = (blue & IVAS_CURSOR_MASK) << 4;
   color = color | (green & IVAS_CURSOR_MASK);
   color = color | ((red & IVAS_CURSOR_MASK) >> 4);

   if (function == CURSOR_OFF) {
      IVAScsShape( 0, 128, 1, 0 );
      IVASgphBlinkCursor( 0, 0 );
      }
   else if (function == COLOR_CURSOR) {
      IVAScsColor( color );
      }
   else {
      IVASmouseClear();
      IVASmouseRead( &button, &IVAS_xy, 3, 0 );
      if (IVAS_CURSOR_FORM != -1) {
         x = IVAS_GPH_X(IVAS_xy.IVAS_x);
         y = IVAS_GPH_Y(IVAS_xy.IVAS_y);
         }
      else {
         x = IVAS_GPH_X(IVAS_xy.IVAS_x - ((IVAS_CURSOR_XSIZE-1)/2));
         y = IVAS_GPH_Y(IVAS_xy.IVAS_y + ((IVAS_CURSOR_YSIZE-1)/2));
         if (x < 1)  x = 1;
         if (y < 1)  y = 1;
         }

      IVAScsColor( color );
      IVASgphBlinkCursor( 0, 0 );
      IVASgphSetCursor( 511, 511 );
      IVASmousePut( 511, 511, 3 );
      IVAS_Rate_Off = IVAS_Rate_On = rate;
      if (IVAS_Rate_On == 0) IVAS_Rate_On = 1;
      IVASflush();

      if (form != -1) {   /*  Use standard, fixed-size cursors  */
         x = IVAS_X_GPH(x);
         y = IVAS_Y_GPH(y);
         IVAScsShape( form, 64, 1, 0 );
         IVASgphSetCursor( x, y );
         IVASmousePut( x, y, 3 );
         }
      else {    /*  Use re-sizable cursor (box with pixel in center)  */

         x = IVAS_X_GPH(x + ((xsize-1)/2));
         y = IVAS_Y_GPH(y + ((ysize-1)/2));

         IVAScsShape( 0, 1, 1, 0 );

         ix = 63 - (xsize-1)/2;
         iy = 63 - (ysize-1)/2;
         ix2 = ix + xsize - 1;
         iy2 = iy + ysize - 1;
         IVAScsLine( ix, iy, xsize, 1, 0);
         IVAScsLine( ix2, iy, ysize, 0, 1);
         IVAScsLine( ix2, iy2, xsize, -1, 0);
         IVAScsLine( ix, iy2, ysize, 0, -1);

         /*  Put center pixel in box (if box is big enough)  */
         if (xsize > 4 && ysize > 4) {
	    IVAScsLine( 63, 63, 1, 1, 0);
            }

         IVASgphSetCursor( x, y );
         IVASmousePut( x, y, 3 );
         }

      IVASgphBlinkCursor( IVAS_Rate_On, IVAS_Rate_Off );
      IVAS_CURSOR_FORM = form;
      IVAS_CURSOR_XSIZE = xsize;
      IVAS_CURSOR_YSIZE = ysize;
      }

   IVASflush();
   
   return (SUCCESS);
}
