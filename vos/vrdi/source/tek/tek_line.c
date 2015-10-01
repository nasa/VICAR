/*	TEK_Line - description
 *
 *	Purpose: Read or write a single image line to the Tektronix display
 *               terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 27, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_Line(Unit, function, imp, x, y, length, buf, mask)
 *
 *	Parameter List:
 *		Unit:	  Display device unit number
 *		function: Function code, either READ_LINE or WRITE_LINE
 *		imp:	  Image plane number to access
 *		x:	  X coordinate of start of line
 *		y:	  Y coordinate of line
 *		length:   Number of bytes in the line
 *		buf:	  Byte buffer containing the line to be read/written
 *		mask:	  Bit plane mask for writes.
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

FUNCTION TEK_Line(Unit, function, imp, x, y, length, buf, mask)
int *Unit, function, imp, x, y, length;
unsigned char *buf, mask;
{
  int	status, samp, area[4];
  unsigned char	*dummy;

  if (function == WRITE_LINE)
  {
    TEK_WRITTEN(imp) = TRUE;

    /*  Put the pixel values into the stored image memory plane  */
    for (samp = 0; samp < length; samp++)
    {
      switch (DEV_TYPE)
      {
        case TEK_4237:
          TEK_IMP(imp, x+samp, y) = (TEK_IMP(imp, x+samp, y) & ~mask) |
                                    (buf[samp] & mask);
          break;
        case TEK_3D_LEFT:
          TEK_IMP_3DL(imp, x+samp, y) = (TEK_IMP_3DL(imp, x+samp, y) & ~mask) |
                                        (buf[samp] & mask);
          break;
        case TEK_3D_RIGHT:
          TEK_IMP_3DR(imp, x+samp, y) = (TEK_IMP_3DR(imp, x+samp, y) & ~mask) |
                                        (buf[samp] & mask);
          break;
      }
    }

    if ((imp == TEK_OVERLAY_IMP) && (TEK_OVERLAY_ON == FALSE))
      status = SUCCESS;
    else
    {
      area[LEFT] = x;
      area[RIGHT] = x + length - 1;
      area[TOP] = y;
      area[BOTTOM] = y;
      status = TEK_AREA(Unit, WRITE_AREA, imp, length, area, dummy, mask,
                        TEK_REFRESH_AREA);
    }
  }
  else					/* Function = READ_LINE */
  {
    status = SUCCESS;
    for (samp = 0; samp < length; samp++)
    {
      switch (DEV_TYPE)
      {
        case TEK_4237:
          buf[samp] = TEK_IMP(imp, x+samp, y);
          break;
        case TEK_3D_LEFT:
          buf[samp] = TEK_IMP_3DL(imp, x+samp, y);
          break;
        case TEK_3D_RIGHT:
          buf[samp] = TEK_IMP_3DR(imp, x+samp, y);
          break;
      }
    }
  }
  return (status);
}
