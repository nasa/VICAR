/*	TEK_Pixel - description
 *
 *	Purpose: Read or write a single pixel to the Tektronix display
 *               terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    November 1, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_Pixel(Unit, function, imp, x, y, pixel, mask)
 *
 *	Parameter List:
 *		Unit:	  Display device unit number
 *		function: Function code, either READ_PIXEL or WRITE_PIXEL
 *		imp:	  Image plane number to read/write from/to
 *		x:	  X coordinate of pixel
 *		y:	  Y coordinate of pixel
 *		pixel:	  Value of pixel
 *		mask:	  Bit plane mask for writing
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_Pixel(Unit, function, imp, x, y, pixel, mask)
int *Unit, function, imp, x, y;
unsigned char *pixel, mask;
{
  int	status, area[4], size;
  unsigned char	*dummy;

  if (function == WRITE_PIXEL)
  {
    TEK_WRITTEN(imp) = TRUE;

    /*  Put the pixel value into the stored image memory plane  */
    switch (DEV_TYPE)
    {
      case TEK_4237:
        TEK_IMP(imp, x, y) = (TEK_IMP(imp, x, y) & ~mask) | ((*pixel) & mask);
        break;
      case TEK_3D_LEFT:
        TEK_IMP_3DL(imp, x, y) = (TEK_IMP_3DL(imp, x, y) & ~mask) |
                                 ((*pixel) & mask);
        break;
      case TEK_3D_RIGHT:
        TEK_IMP_3DR(imp, x, y) = (TEK_IMP_3DR(imp, x, y) & ~mask) |
                                 ((*pixel) & mask);
        break;
    }

    if ((imp == TEK_OVERLAY_IMP) && (TEK_OVERLAY_ON == FALSE))
      status = SUCCESS;
    else
    {
      area[LEFT] = x;
      area[RIGHT] = x;
      area[TOP] = y;
      area[BOTTOM] = y;
      status = TEK_AREA(Unit, WRITE_AREA, imp, size, area, dummy, mask,
                        TEK_REFRESH_AREA);
    }
  }
  else					/* Function = READ_PIXEL */
  {
    status = SUCCESS;
    switch (DEV_TYPE)
    {
      case TEK_4237:
        *pixel = TEK_IMP(imp, x, y);
        break;
      case TEK_3D_LEFT:
        *pixel = TEK_IMP_3DL(imp, x, y);
        break;
      case TEK_3D_RIGHT:
        *pixel = TEK_IMP_3DR(imp, x, y);
        break;
    }
  }
  return (status);
}
