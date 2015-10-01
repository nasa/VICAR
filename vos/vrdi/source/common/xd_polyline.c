/*	XD_Polyline - Draws a series of lines with clipping
 *
 *	Purpose:
 *
 *	Written by:  R. A. Mortensen
 *	Date:	     September 19, 1986
 *
 *	Calling Sequence:
 *
 *		STATUS = XD_Polyline( Unit, Imp, NPts, X, Y, Value, Mask )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number (by reference)
 *		Imp:	Image memory plane number (by value)
 *		NPts:	Number of coordinates (by value)
 *		X:	X coordinates (by reference)
 *		Y:	Y coordinates (by reference)
 *		Value:	Pixel value (by reference)
 *		Mask:	Pixel mask (by value)
 *
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"


FUNCTION int XD_Polyline( Unit, Imp, NPts, X, Y, Value, Mask )
int	*Unit, Imp, NPts, *X, *Y, Mask;
char	*Value;
{
  int	status, count, ptr, *XClip, *YClip, clip;

  XClip = (int *) malloc( sizeof(int)*NPts );
  YClip = (int *) malloc( sizeof(int)*NPts );
  if ((XClip == 0) || (YClip == 0)) {
    status = MEMORY_ERROR;
  }
  else {
    status = SUCCESS;
      
    ptr = 0;
    for (count = 1; count < NPts; count++ ) {
      XClip[ptr]   = X[count-1];
      YClip[ptr]   = Y[count-1];
      XClip[ptr+1] = X[count];
      YClip[ptr+1] = Y[count];

      clip = XD_Clip( &XClip[ptr], &YClip[ptr], AW(Imp) );
      if (clip != -1) {
	ptr++;
	if ( BIT_TEST(clip,1) ) {
	  status = XD_Device_Interface( Unit, WRITE_VECTOR,
				Imp, ptr+1, XClip, YClip, *Value, Mask );

	  if ( status == DEVICE_CANNOT_DO_IT ) {
	    status = XD_Draw_Vector( Unit, Imp, ptr+1, XClip, 
                                     YClip, Value, Mask );
	    if (status != SUCCESS) break;
	  }
	  ptr = 0;
	}
      }
      if (status != SUCCESS) break;
    }

    if (ptr > 0) {
      status = XD_Device_Interface( Unit, WRITE_VECTOR,
                             Imp, ptr+1, XClip, YClip, *Value, Mask );

      if ( status == DEVICE_CANNOT_DO_IT ) {
	status = XD_Draw_Vector( Unit, Imp, ptr+1, XClip, 
                                 YClip, Value, Mask );
      }
    }
  }

  free( XClip );
  free( YClip );
  return (status );
}

FUNCTION int XD_Draw_Vector( Unit, Imp, NPts, X, Y, Value, Mask )
int	*Unit, Imp, NPts, *X, *Y, *Mask;
char	*Value;
{
  int dx, dy, incr1, incr2, d, x, y;
  int XStart, YStart, XEnd, YEnd, Offset, i, status;

  for ( i = 0;i < NPts - 1; i++ ) {

    Offset = 1;
    XStart = X[i];
    XEnd   = X[i+1];
    YStart = Y[i];
    YEnd   = Y[i+1];
    dx = abs( XEnd - XStart );
    dy = abs( YEnd - YStart );

    if ( dx < dy ) {
      if ( YStart > YEnd ) {
	XStart = X[i+1], XEnd = X[i];
	YStart = Y[i+1], YEnd = Y[i];
      }
      status = XD_Device_Interface( Unit, WRITE_PIXEL, Imp, XStart, 
                             YStart, Value, Mask );
      if ( status != SUCCESS ) break;
      x = XStart;
      if ( XStart > XEnd ) Offset = -1;
      d = 2 * dx - dy;
      incr1 = 2 * dx;
      incr2 = 2 * ( dx - dy );
      for ( y = YStart + 1; y <= YEnd; y++ ) {
	if ( d < 0 ) {
	  d = d + incr1;
	} else {
	  x = x + Offset;
	  d = d + incr2;
	}
	status = XD_Device_Interface( Unit, WRITE_PIXEL, Imp, x, 
	                       y, Value, Mask );
	if ( status != SUCCESS ) break;
      }
    } else {
      if ( XStart > XEnd ) {
	XStart = X[i+1], XEnd = X[i];
	YStart = Y[i+1], YEnd = Y[i];
      }
      status = XD_Device_Interface( Unit, WRITE_PIXEL, Imp, XStart, 
	                       YStart, Value, Mask );
      if ( status != SUCCESS ) break;
      y = YStart;
      if ( YStart > YEnd) Offset = -1;
      d = 2 * dy - dx;
      incr1 = 2 * dy;
      incr2 = 2 * ( dy - dx );

      for ( x = XStart + 1; x <= XEnd; x++ ) {
	if ( d < 0 ) {
	  d = d + incr1;
	} else {
	  y = y + Offset;
	  d = d + incr2;
	}
	status = XD_Device_Interface( Unit, WRITE_PIXEL, Imp, x, 
	                       y, Value, Mask );
	if ( status != SUCCESS ) break;
      }
    }
  }
  return(status);
}
