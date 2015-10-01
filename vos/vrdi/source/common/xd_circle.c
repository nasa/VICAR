/*	XD_Circle - Draws a circle clipped at the access window
 *
 *	Purpose:
 *
 *	Written by:  R. A. Mortensen
 *	Date:	     September 19, 1986
 *
 *	Calling Sequence:
 *
 *		STATUS = XD_Circle( Unit, Imp, X, Y, R, Value )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number (by reference)
 *		Imp:	Image memory plane number (by value)
 *		X:	X coordinate of circle center (by value)
 *		Y:	Y coordinate of circle center (by value)
 *		R:	Radius of circle (by value)
 *		Value:	Pixel value (by reference)
 *
 *	Possible Error Codes:
 *
 */
#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"


FUNCTION int XD_Circle( Unit, Imp, XCenter, YCenter, Radius, Value, Mask )
int	*Unit, Imp, XCenter, YCenter, Radius;
char	*Value, Mask;

   {
   int	x, y, d, status;

   x = 0;
   y = Radius;
   d = 3 - 2*Radius;
   while ( x < y ) {
      status = XD_Circle_Points( Unit, Imp, Value, Mask,
					XCenter, YCenter, x, y );
      if (status != SUCCESS) break;

      if (d < 0) {
	 d = d + 4*x + 6;
	 }
      else {
	 d = d + 4*(x-y) + 10;
	 y -= 1;
	 }
      x += 1;
      }

   if (x == y) {
      status = XD_Circle_Points( Unit, Imp, Value, Mask, XCenter, YCenter, x, y );
      }

   return (status);
   }


FUNCTION int XD_Circle_Points( Unit, Imp, Value, Mask, XCenter, YCenter, x, y )
int	*Unit, Imp, XCenter, YCenter, x, y;
char	*Value, Mask;

   {
   int	status, XPos, YPos;

   status = SUCCESS;
   
   XPos = XCenter + x, YPos = YCenter + y;
   if (XD_Out_Codes(XPos, YPos, AW(Imp)) == 0) {
      status = XD_Device_Interface( Unit, WRITE_PIXEL,
				    Imp, XPos, YPos, Value, Mask );
      }
   if (status != SUCCESS) return (status);

   XPos = XCenter + y, YPos = YCenter + x;
   if (XD_Out_Codes(XPos, YPos, AW(Imp)) == 0) {
      status = XD_Device_Interface( Unit, WRITE_PIXEL,
				    Imp, XPos, YPos, Value, Mask );
      }
   if (status != SUCCESS) return (status);

   XPos = XCenter + y, YPos = YCenter - x;
   if (XD_Out_Codes(XPos, YPos, AW(Imp)) == 0) {
      status = XD_Device_Interface( Unit, WRITE_PIXEL,
				    Imp, XPos, YPos, Value, Mask );
      }
   if (status != SUCCESS) return (status);

   XPos = XCenter + x, YPos = YCenter - y;
   if (XD_Out_Codes(XPos, YPos, AW(Imp)) == 0) {
      status = XD_Device_Interface( Unit, WRITE_PIXEL,
				    Imp, XPos, YPos, Value, Mask );
      }
   if (status != SUCCESS) return (status);

   XPos = XCenter - x, YPos = YCenter - y;
   if (XD_Out_Codes(XPos, YPos, AW(Imp)) == 0) {
      status = XD_Device_Interface( Unit, WRITE_PIXEL,
				    Imp, XPos, YPos, Value, Mask );
      }
   if (status != SUCCESS) return (status);

   XPos = XCenter - y, YPos = YCenter - x;
   if (XD_Out_Codes(XPos, YPos, AW(Imp)) == 0) {
      status = XD_Device_Interface( Unit, WRITE_PIXEL,
				    Imp, XPos, YPos, Value, Mask );
      }
   if (status != SUCCESS) return (status);

   XPos = XCenter - y, YPos = YCenter + x;
   if (XD_Out_Codes(XPos, YPos, AW(Imp)) == 0) {
      status = XD_Device_Interface( Unit, WRITE_PIXEL,
				    Imp, XPos, YPos, Value, Mask );
      }
   if (status != SUCCESS) return (status);

   XPos = XCenter - x, YPos = YCenter + y;
   if (XD_Out_Codes(XPos, YPos, AW(Imp)) == 0) {
      status = XD_Device_Interface( Unit, WRITE_PIXEL,
				    Imp, XPos, YPos, Value, Mask );
      }
   return (status);
   }
